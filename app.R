# Own.it — v3 ─────────────────────────────────────────────────────────────────
# Athlete Passport · Coach Roster / Lineups / Schedule
# ──────────────────────────────────────────────────────────────────────────────

library(shiny)

# ── Helpers ───────────────────────────────────────────────────────────────────
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
new_id  <- function() paste0("usr_", paste0(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
gen_pin <- function() paste0(sample(0:9, 4, replace = TRUE), collapse = "")

# ── Sport roster positions ────────────────────────────────────────────────────
sport_positions <- list(
  Basketball = c("Point Guard","Shooting Guard","Small Forward","Power Forward","Center"),
  Football   = c("QB","RB","WR1","WR2","TE","LT","LG","C","RG","RT",
                 "DE1","DE2","DT1","DT2","MLB","OLB1","OLB2","CB1","CB2","FS","SS"),
  Soccer     = c("GK","LB","CB1","CB2","RB","LM","CM1","CM2","RM","SS","ST"),
  Baseball   = c("Pitcher","Catcher","1B","2B","3B","SS","LF","CF","RF"),
  Softball   = c("Pitcher","Catcher","1B","2B","3B","SS","LF","CF","RF"),
  Volleyball = c("Setter","Libero","OH1","OH2","MB1","MB2","Opposite"),
  Tennis     = c("Singles 1","Singles 2","Singles 3","Doubles 1A","Doubles 1B","Doubles 2A","Doubles 2B"),
  Golf       = c("1st","2nd","3rd","4th","5th","6th","7th","8th","Alternate"),
  Swimming   = c("Freestyle","Backstroke","Breaststroke","Butterfly","IM","Relay 1","Relay 2","Relay 3","Relay 4"),
  Wrestling  = c("106","113","120","126","132","138","144","150","157","165","175","190","215","285"),
  Lacrosse   = c("GK","Def 1","Def 2","Def 3","Mid 1","Mid 2","Mid 3","Attack 1","Attack 2","Attack 3"),
  Hockey     = c("GK","LD","RD","LW","C","RW"),
  Track      = c("Sprinter 1","Sprinter 2","Mid-Dist 1","Mid-Dist 2",
                 "Long Dist","Relay 1","Relay 2","Relay 3","Relay 4"),
  CrossCountry = c("Runner 1","Runner 2","Runner 3","Runner 4","Runner 5","Runner 6","Runner 7"),
  Gymnastics = c("All-Around","Vault","Bars","Beam","Floor"),
  Cheer      = c("Flyer","Base 1","Base 2","Back Spot","Tumbler","Stunter"),
  Other      = paste0("Player ", 1:11)
)

# ── Sport-specific game stat fields ───────────────────────────────────────────
sport_stats <- list(
  Basketball   = c("Points","Assists","Rebounds"),
  Football     = c("Yards","TDs","Tackles"),
  Soccer       = c("Goals","Assists","Saves"),
  Baseball     = c("Hits","RBIs","Strikeouts"),
  Softball     = c("Hits","RBIs","Strikeouts"),
  Volleyball   = c("Kills","Aces","Digs"),
  Tennis       = c("Games Won","Sets Won","Aces"),
  Golf         = c("Score","Fairways Hit","Putts"),
  Swimming     = c("Place","Events","Best Time"),
  Wrestling    = c("Takedowns","Pins","Points"),
  Lacrosse     = c("Goals","Assists","Ground Balls"),
  Hockey       = c("Goals","Assists","Saves"),
  Track        = c("Place","Events","Personal Best"),
  CrossCountry = c("Place","Time (min)","PR"),
  Gymnastics   = c("Score","Events","Difficulty"),
  Cheer        = c("Score","Stunts","Tumbling"),
  Other        = c("Stat 1","Stat 2","Stat 3")
)

# ── Persistence ───────────────────────────────────────────────────────────────
data_dir  <- if (.Platform$OS.type == "windows") {
  "C:/Users/Jbenz/Desktop/own.it/data"
} else {
  "/srv/shiny-server/own-it/data"
}
data_file <- file.path(data_dir, "athletes.rds")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

athlete_store <- new.env(hash = TRUE, parent = emptyenv())

.load_store <- function(f) {
  saved <- readRDS(f)
  if (!is.environment(saved)) stop("data file is not an environment")
  saved
}
if (file.exists(data_file)) {
  saved <- tryCatch(.load_store(data_file), error = function(e) {
    bak <- paste0(data_file, ".bak")
    if (file.exists(bak)) {
      message("Main data file unreadable (", e$message, ") — restoring from backup.")
      tryCatch(.load_store(bak), error = function(e2) NULL)
    } else NULL
  })
  if (is.environment(saved))
    for (nm in ls(saved)) assign(nm, get(nm, envir = saved), envir = athlete_store)
}

# Crash-safe save: write to a temp file, verify it reads back, keep a backup of
# the last-good file, then swap it in. If the process dies mid-write the original
# data file (or its .bak) stays intact, so a roster can't be wiped by a bad write.
save_athlete <- function(ath) {
  assign(ath$id, ath, envir = athlete_store)
  tmp <- paste0(data_file, ".tmp")
  ok <- tryCatch({
    saveRDS(athlete_store, tmp)
    is.environment(readRDS(tmp))   # verify the snapshot is fully readable
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    if (file.exists(tmp)) unlink(tmp)
    warning("save_athlete: temp write failed, keeping previous data file")
    return(invisible(FALSE))
  }
  if (file.exists(data_file))
    file.copy(data_file, paste0(data_file, ".bak"), overwrite = TRUE)
  file.copy(tmp, data_file, overwrite = TRUE)
  unlink(tmp)
  invisible(TRUE)
}
get_athlete <- function(id) {
  id <- trimws(id %||% "")
  if (!nzchar(id) || !exists(id, envir = athlete_store)) return(NULL)
  get(id, envir = athlete_store)
}
find_by_name <- function(name) {
  name <- tolower(trimws(name))
  for (id in ls(athlete_store)) {
    a <- get(id, envir = athlete_store)
    if (tolower(trimws(a$name %||% "")) == name) return(a)
  }
  NULL
}
# Match on name AND pin so two people with the same name don't collide.
find_by_name_pin <- function(name, pin) {
  name <- tolower(trimws(name)); pin <- trimws(pin)
  for (id in ls(athlete_store)) {
    a <- get(id, envir = athlete_store)
    if (tolower(trimws(a$name %||% "")) == name &&
        trimws(a$pin %||% "") == pin) return(a)
  }
  NULL
}
# Honest streak: number of consecutive calendar days with a check-in, ending
# today or yesterday. Entries without an ISO date (older data) are ignored.
compute_streak <- function(journals) {
  isos  <- unlist(lapply(journals %||% list(), function(j) j$iso %||% NA))
  dates <- suppressWarnings(as.Date(isos))
  dates <- sort(unique(dates[!is.na(dates)]), decreasing = TRUE)
  if (!length(dates)) return(0)
  today <- Sys.Date()
  if (as.numeric(today - dates[1]) > 1) return(0)  # streak broken
  streak <- 1L
  for (i in seq_len(length(dates) - 1)) {
    if (as.numeric(dates[i] - dates[i + 1]) == 1) streak <- streak + 1L else break
  }
  streak
}

# ── Brute-force protection ──────────────────────────────────────────────────
# Lock a name out after too many wrong PINs. State is keyed by lowercased name
# and shared across sessions in this process.
LOGIN_MAX_TRIES <- 5L      # wrong PINs allowed before a lockout
LOGIN_LOCK_SECS <- 300     # lockout length in seconds (5 minutes)
.login_state <- new.env(parent = emptyenv())
login_locked_secs <- function(key) {       # seconds left on lockout, 0 if none
  st <- .login_state[[key]]
  if (is.null(st) || is.null(st$until)) return(0)
  rem <- as.numeric(st$until) - as.numeric(Sys.time())
  if (rem > 0) ceiling(rem) else 0
}
login_note_fail <- function(key) {          # record a wrong attempt
  st <- .login_state[[key]] %||% list(count = 0L, until = NULL)
  st$count <- (st$count %||% 0L) + 1L
  if (st$count >= LOGIN_MAX_TRIES) { st$until <- Sys.time() + LOGIN_LOCK_SECS; st$count <- 0L }
  .login_state[[key]] <- st
  LOGIN_MAX_TRIES - st$count                # tries remaining (<=0 means now locked)
}
login_clear <- function(key) .login_state[[key]] <- NULL
lock_msg <- function(secs) {
  m <- ceiling(secs / 60)
  paste0("Too many incorrect PINs. Try again in ", m, if (m == 1) " minute." else " minutes.")
}

get_all_athletes <- function() {
  ids  <- ls(athlete_store)
  aths <- lapply(ids, function(id) get(id, envir = athlete_store))
  Filter(function(a) is.null(a$role) || a$role != "coach", aths)
}
get_coach <- function() {
  for (id in ls(athlete_store)) {
    a <- get(id, envir = athlete_store)
    if (identical(a$role, "coach")) return(a)
  }
  NULL
}

athlete_status <- function(ath) {
  s <- tryCatch(as.numeric(ath$streak %||% 0)[1L], error = function(e) 0)
  if (is.na(s) || !is.numeric(s)) s <- 0
  if (s >= 5) return(list(dot = "🟢", label = "On fire",   bg = "#1a2e0a", border = "#2a4a10", color = "#C8F04B"))
  if (s >= 2) return(list(dot = "🟡", label = "Active",    bg = "#1f1a08", border = "#4a3a10", color = "#f59e0b"))
  return(         list(dot = "🔴", label = "Needs work", bg = "#2a0a0a", border = "#450a0a", color = "#f87171"))
}

calc_passport <- function(games, journals, streak) {
  streak <- tryCatch(as.numeric(streak)[1L], error = function(e) 0)
  if (is.na(streak) || !is.numeric(streak)) streak <- 0
  perf <- tryCatch({
    if (is.null(games) || !is.data.frame(games) || nrow(games) == 0) 70
    else { r <- suppressWarnings(as.numeric(games$Rating)); m <- mean(r, na.rm=TRUE); if (is.nan(m)) 70 else m }
  }, error = function(e) 70)
  well <- tryCatch({
    if (!is.list(journals) || !length(journals)) 70
    else mean(vapply(journals, function(j) {
      s <- suppressWarnings(as.numeric(j$score)[1L]); if (is.na(s)) 7 else s
    }, numeric(1)), na.rm=TRUE) * 10
  }, error = function(e) 70)
  round(max(0, min(100, perf * 0.55 + well * 0.30 + min(100, streak * 10) * 0.15)))
}

sample_games <- function(sport) {
  if (sport == "Football")
    data.frame(Date=c("Jun 8","Jun 3","May 28"),Opponent=c("Westview","Lake Park","Central"),
               Result=c("W 35-21","W 28-14","L 10-17"),Yards=c(287,198,142),
               TDs=c(3,2,1),Rating=c(97,84,58),stringsAsFactors=FALSE)
  else if (sport == "Soccer")
    data.frame(Date=c("Jun 8","Jun 3","May 28"),Opponent=c("Westview","Lake Park","Central"),
               Result=c("W 3-1","W 2-1","L 0-2"),Goals=c(2,1,0),
               Assists=c(1,2,0),Rating=c(91,79,42),stringsAsFactors=FALSE)
  else
    data.frame(Date=c("Jun 8","Jun 3","May 28"),Opponent=c("Westview","Lake Park","Central"),
               Result=c("W 72-61","W 58-54","L 49-55"),Points=c(24,18,12),
               Assists=c(7,5,4),Rebounds=c(9,11,6),Rating=c(94,82,61),stringsAsFactors=FALSE)
}

# ── CSS ───────────────────────────────────────────────────────────────────────
css <- "
@import url('https://fonts.googleapis.com/css2?family=Barlow:wght@400;500;600;700;800;900&display=swap');
* { font-family: 'Barlow', sans-serif !important }
body { background: #0a0c10 !important; color: #f0f2f5 !important; margin: 0 }
.page-wrap { max-width: 1100px; margin: 0 auto; padding: 2rem 1.5rem }
.navbar { background: #0d1018 !important; border-bottom: 1px solid #1e2330 }
.navbar-brand { color: #C8F04B !important; font-weight: 900 !important; font-size: 1.4rem !important }
.nav-tabs .nav-link { color: #6b7a99 !important; font-weight: 600 !important; font-size: 13px !important; border: none !important; padding: .8rem 1.1rem !important }
.nav-tabs .nav-link.active { color: #C8F04B !important; border-bottom: 2px solid #C8F04B !important; background: transparent !important }
.nav-tabs { border-bottom: 1px solid #1e2330 !important }
.card, .well { background: #13171f !important; border: 1px solid #1e2330 !important; border-radius: 14px !important; color: #f0f2f5 !important }
.stat-card { background: #13171f; border: 1px solid #1e2330; border-radius: 14px; padding: 1.25rem 1.5rem; height: 100% }
.stat-label { font-size: 11px; color: #6b7a99; text-transform: uppercase; font-weight: 700; letter-spacing: .08em; margin-bottom: 6px }
.stat-value { font-size: 34px; color: #C8F04B; font-weight: 900; line-height: 1 }
.stat-sub { font-size: 12px; color: #6b7a99; margin-top: 5px }
.btn-own { background: #C8F04B !important; color: #0a0c10 !important; border: none !important; font-weight: 800 !important; font-size: 13px !important; padding: 10px 22px !important; border-radius: 10px !important }
.btn-own:hover { background: #b8e03b !important }
.btn-ghost { background: transparent !important; color: #f0f2f5 !important; border: 1px solid #2a3040 !important; font-weight: 600 !important; font-size: 13px !important; padding: 9px 20px !important; border-radius: 10px !important }
.btn-danger-sm { background: transparent !important; color: #f87171 !important; border: 1px solid #450a0a !important; font-weight: 600 !important; font-size: 11px !important; padding: 4px 10px !important; border-radius: 6px !important }
.form-control, .form-select { background: #1a1f2b !important; color: #f0f2f5 !important; border: 1px solid #2a3040 !important; border-radius: 8px !important; font-size: 13px !important }
.form-control:focus, .form-select:focus { border-color: #C8F04B !important; box-shadow: 0 0 0 3px rgba(200,240,75,.15) !important }
.form-label, .control-label { font-size: 13px !important; font-weight: 600 !important; color: #9ba8c0 !important }
.login-wrap { min-height: 100vh; display: flex; align-items: center; justify-content: center; padding: 2rem; background: #0a0c10; background-image: radial-gradient(ellipse at 50% 0%, rgba(200,240,75,.06) 0%, transparent 60%) }
.login-card { background: #111518; border: 1px solid rgba(200,240,75,.12); border-radius: 24px; padding: 3rem 2.75rem; width: 100%; max-width: 520px; box-shadow: 0 32px 80px rgba(0,0,0,.6) }
.login-logo { font-size: 3.6rem; font-weight: 900; color: #C8F04B; letter-spacing: -.06em; line-height: 1; text-shadow: 0 0 60px rgba(200,240,75,.4) }
.login-sub { color: #5a6478; font-size: 13px; margin-top: 6px; letter-spacing: .04em; font-weight: 600; text-transform: uppercase }
.passport-hero { background: linear-gradient(135deg,#1a2e0a,#0d1f0a,#13171f); border: 2px solid #C8F04B; border-radius: 18px; padding: 1.75rem }
.passport-score { font-size: 72px; font-weight: 900; color: #C8F04B; letter-spacing: -.05em; line-height: 1 }
.journal-entry { background: #1a1f2b; border: 1px solid #1e2330; border-radius: 10px; padding: .9rem 1.1rem; margin-bottom: .6rem }
.je-date { font-size: 11px; color: #6b7a99; font-weight: 700; text-transform: uppercase; letter-spacing: .05em }
.je-note { font-size: 13px; color: #d1d9e8; margin-top: 5px; line-height: 1.6 }
.alert-success { background: #1a2e0a; border: 1px solid #2a4a10; color: #C8F04B; border-radius: 10px; padding: 10px 14px; font-size: 13px }
.alert-danger  { background: #2a0a0a; border: 1px solid #450a0a; color: #f87171;  border-radius: 10px; padding: 10px 14px; font-size: 13px }
.alert-own     { background: #0f1f3a; border: 1px solid #1e3a5f; color: #60a5fa;  border-radius: 10px; padding: 10px 14px; font-size: 13px }
.pin-card { background: #0f1a0a; border: 2px solid #C8F04B; border-radius: 16px; padding: 1.5rem; margin-bottom: 1.5rem; text-align: center }
.pin-display { font-size: 52px; font-weight: 900; color: #C8F04B; letter-spacing: .35em; font-family: monospace; margin: .5rem 0 }
table { font-size: 13px !important; color: #f0f2f5 !important; background: #13171f !important; width: 100% !important }
table thead th { font-weight: 700 !important; font-size: 11px !important; text-transform: uppercase; color: #6b7a99 !important; border-bottom: 1px solid #1e2330 !important; background: #13171f !important; padding: 10px 14px !important }
table tbody td { padding: 10px 14px !important; border-bottom: 1px solid #1a1f2b !important; background: #13171f !important; color: #f0f2f5 !important }
table tbody tr:hover td { background: #1a1f2b !important }
input[type=range] { accent-color: #C8F04B }
hr.own { border: none; border-top: 1px solid #1e2330; margin: 1.5rem 0 }
.athlete-row { background: #13171f; border: 1px solid #1e2330; border-radius: 14px; padding: 1.2rem 1.5rem; margin-bottom: .75rem; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 1rem }
.athlete-row:hover { border-color: #2a3347 }
.ath-name { font-size: 16px; font-weight: 800; color: #f0f2f5 }
.ath-sub  { font-size: 12px; color: #6b7a99; margin-top: 3px }
.ath-pin  { font-size: 11px; color: #5a6478; margin-top: 3px; font-weight: 700 }
.ath-stat-val { font-size: 22px; font-weight: 900; color: #C8F04B; line-height: 1 }
.ath-stat-lbl { font-size: 10px; color: #6b7a99; font-weight: 700; text-transform: uppercase; margin-top: 3px }
.status-badge { border-radius: 8px; padding: 4px 12px; font-size: 11px; font-weight: 800; letter-spacing: .04em }
.coach-badge { display:inline-block; background:#0f1f3a; border:1px solid #1e3a5f; color:#60a5fa; border-radius:8px; padding:3px 10px; font-size:11px; font-weight:700; margin-left:.5rem }
.pos-slot { background: #1a1f2b; border-radius: 8px; padding: .6rem .9rem }
.pos-label { font-size: 10px; color: #6b7a99; font-weight: 700; text-transform: uppercase; letter-spacing: .05em; margin-bottom: 3px }
.pos-player { font-size: 13px; color: #f0f2f5; font-weight: 700 }
.pos-empty { font-size: 13px; color: #3a4050; font-weight: 600 }
.lineup-card { background: #13171f; border: 1px solid #1e2330; border-radius: 14px; padding: 1.2rem 1.5rem; margin-bottom: 1rem }
.sched-row { background: #13171f; border: 1px solid #1e2330; border-radius: 12px; padding: 1rem 1.5rem; margin-bottom: .75rem; display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:.75rem }
.coach-header { padding: 1.5rem 0 .25rem }
.coach-date { font-size: 12px; color: #6b7a99; font-weight: 700; letter-spacing: .08em; text-transform: uppercase }
.coach-title { font-size: 28px; font-weight: 900; color: #f0f2f5; margin-top: .2rem; line-height: 1.1 }
.pill-nav { display:flex; gap:.5rem; padding: 1.5rem 0 1.75rem; flex-wrap: wrap; }
.pill { display:inline-block; padding: 9px 20px; border-radius: 9999px; font-size: 13px; font-weight: 700; cursor: pointer; border: 1px solid #1e2330; background: #13171f; color: #6b7a99; transition: all .15s; white-space: nowrap; user-select:none; }
.pill.active { background: #C8F04B; color: #0a0c10; border-color: #C8F04B; }
.pill:hover:not(.active) { border-color: #2a3347; color: #f0f2f5; }
.pin-roster-row { background: #13171f; border: 1px solid #1e2330; border-radius: 14px; padding: 1rem 1.5rem; margin-bottom: .65rem; display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:1rem; }
.pin-roster-row:hover { border-color: #2a3347; }
.pin-big { font-size: 28px; font-weight: 900; color: #C8F04B; letter-spacing: .3em; font-family: monospace; }
.cal-grid { border: 1px solid #1e2330; border-radius: 12px; overflow: hidden; margin-bottom: 1.5rem; }
.cal-header-row { display:flex; background:#13171f; }
.cal-row { display:flex; }
.cal-cell { flex:1; min-height:80px; border-right:1px solid #1e2330; border-bottom:1px solid #1e2330; padding:6px 7px; box-sizing:border-box; background:#0d0f14; overflow:hidden; }
.cal-cell:last-child { border-right:none; }
.cal-row:last-child .cal-cell { border-bottom:none; }
.cal-cell.empty { background:#0a0c10; }
.cal-cell.cal-today { background:#101820; }
.cal-day-num { font-size:11px; font-weight:700; color:#3a4560; margin-bottom:3px; }
.cal-today .cal-day-num { color:#C8F04B; background:#C8F04B22; border-radius:99px; width:20px; height:20px; display:flex; align-items:center; justify-content:center; }
.cal-event { border-radius:4px; padding:2px 5px; margin-bottom:2px; }
.cal-event-title { font-size:10px; font-weight:800; line-height:1.3; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.cal-event-sub { font-size:9px; color:#6b7a99; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.cal-head-cell { flex:1; text-align:center; font-size:11px; font-weight:700; color:#6b7a99; text-transform:uppercase; letter-spacing:.06em; padding:8px 4px; border-right:1px solid #1e2330; }
.cal-head-cell:last-child { border-right:none; }
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- tagList(
  tags$head(tags$style(HTML(css))),
  uiOutput("main_ui")
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  page             <- reactiveVal("login")
  athlete_id       <- reactiveVal(NULL)
  games_rv         <- reactiveVal(NULL)
  journals_rv      <- reactiveVal(list())
  streak_rv        <- reactiveVal(0)
  new_player_rv    <- reactiveVal(NULL)   # list(name, pin) after coach adds player
  coach_sel_ath_rv <- reactiveVal(NULL)   # athlete ID selected in coach profile view
  coach_tab_rv     <- reactiveVal("roster") # active coach tab
  athlete_tab_rv   <- reactiveVal("passport") # active athlete tab
  sched_month_rv   <- reactiveVal(format(Sys.Date(), "%Y-%m"))

  athlete  <- reactive(get_athlete(athlete_id() %||% ""))
  passport <- reactive(calc_passport(games_rv(), journals_rv(), streak_rv()))

  # ── ROUTER ────────────────────────────────────────────────────────────────
  output$main_ui <- renderUI({
    p <- page()
    coach_reset_rv()   # reactive dependency so reset forces redraw
    if (p == "athlete")     return(athlete_screen())
    if (p == "coach")       return(coach_screen())
    if (p == "coach_login") return(coach_login_screen())
    login_screen()
  })

  # ── ATHLETE LOGIN / SIGNUP ────────────────────────────────────────────────
  login_screen <- function() {
    div(class = "login-wrap",
      div(class = "login-card",
        div(style = "text-align:center;margin-bottom:2rem;",
          div(class = "login-logo", "Stattrakker"),
          div(class = "login-sub",  "The Athlete Passport")),

        # ── Tab switcher ──
        div(style = "display:flex;border:1px solid #1e2330;border-radius:12px;overflow:hidden;margin-bottom:1.75rem;",
          tags$button(id = "tab_login_btn",
            style = "flex:1;padding:10px;font-size:13px;font-weight:700;cursor:pointer;border:none;border-radius:0;background:#C8F04B;color:#0a0c10;",
            onclick = "document.getElementById('tab_login').style.display='block';
                       document.getElementById('tab_signup').style.display='none';
                       this.style.background='#C8F04B';this.style.color='#0a0c10';
                       document.getElementById('tab_signup_btn').style.background='transparent';
                       document.getElementById('tab_signup_btn').style.color='#6b7a99';",
            "Log In"),
          tags$button(id = "tab_signup_btn",
            style = "flex:1;padding:10px;font-size:13px;font-weight:700;cursor:pointer;border:none;border-radius:0;background:transparent;color:#6b7a99;",
            onclick = "document.getElementById('tab_signup').style.display='block';
                       document.getElementById('tab_login').style.display='none';
                       this.style.background='#C8F04B';this.style.color='#0a0c10';
                       document.getElementById('tab_login_btn').style.background='transparent';
                       document.getElementById('tab_login_btn').style.color='#6b7a99';",
            "Sign Up")
        ),

        # ── LOG IN tab ──
        div(id = "tab_login",
          uiOutput("login_msg"),
          div(style = "margin-bottom:1rem;",
            tags$label(class = "control-label", "Full Name"),
            textInput("inp_name", NULL, placeholder = "Your name")),
          div(style = "margin-bottom:1.5rem;",
            tags$label(class = "control-label", "PIN"),
            passwordInput("inp_pin", NULL, placeholder = "Your PIN")),
          actionButton("btn_login", "Enter", class = "btn-own",
            style = "width:100%;font-size:15px;padding:14px;margin-bottom:1rem;"),
          div(style="font-size:11px;color:#4a5268;text-align:center;",
            "PIN given by your coach, or the one you created when you signed up.")
        ),

        # ── SIGN UP tab ──
        div(id = "tab_signup", style = "display:none;",
          uiOutput("signup_msg"),
          div(style = "margin-bottom:.85rem;",
            tags$label(class = "control-label", "Full Name"),
            textInput("su_name", NULL, placeholder = "Your full name")),
          fluidRow(
            column(6,
              tags$label(class = "control-label", "Sport"),
              selectInput("su_sport", NULL,
                choices = names(sport_positions))),
            column(6,
              tags$label(class = "control-label", "Position"),
              selectInput("su_pos", NULL, choices = sport_positions[["Basketball"]]))
          ),
          div(style = "margin-bottom:.85rem;",
            tags$label(class = "control-label", "Level"),
            selectInput("su_level", NULL,
              choices = c("Youth","Middle School","High School","College","Semi-Pro","Pro"))),
          div(style = "margin-bottom:1.5rem;",
            tags$label(class = "control-label", "Choose a PIN (4 digits)"),
            passwordInput("su_pin", NULL, placeholder = "4-digit PIN you'll use to log in")),
          actionButton("btn_signup", "Create My Profile", class = "btn-own",
            style = "width:100%;font-size:15px;padding:14px;margin-bottom:1rem;"),
          div(style="font-size:11px;color:#4a5268;text-align:center;",
            "Free during beta. Your passport is yours — no coach required.")
        ),

        tags$hr(style="border:none;border-top:1px solid #1e2330;margin:1.25rem 0;"),
        div(style = "text-align:center;",
          actionLink("go_coach_login", "Coach login →",
            style = "color:#5a6478;font-size:12px;font-weight:700;text-decoration:none;"))
      )
    )
  }

  observeEvent(input$go_coach_login, { page("coach_login") })

  # ── LOG IN handler ────────────────────────────────────────────────────────
  observeEvent(input$btn_login, {
    name <- trimws(input$inp_name %||% "")
    pin  <- trimws(input$inp_pin  %||% "")
    if (!nzchar(name)) {
      output$login_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;","Enter your name."))
      return()
    }
    if (!nzchar(pin)) {
      output$login_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;","Enter your PIN."))
      return()
    }
    lock_key <- paste0("ath:", tolower(name))
    locked <- login_locked_secs(lock_key)
    if (locked > 0) {
      output$login_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;", lock_msg(locked)))
      return()
    }
    existing <- find_by_name_pin(name, pin)
    if (is.null(existing) || identical(existing$role, "coach")) {
      left    <- login_note_fail(lock_key)
      nowlock <- login_locked_secs(lock_key)
      msg  <- if (nowlock > 0) lock_msg(nowlock)
              else paste0("No account found with that name and PIN. ",
                          left, " attempt", if (left == 1) "" else "s", " left before a temporary lockout.")
      output$login_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;", msg))
      return()
    }
    login_clear(lock_key)
    athlete_id(existing$id)
    g <- existing$games
    if (is.null(g) || (is.data.frame(g) && nrow(g) == 0)) g <- NULL
    games_rv(g)
    journals_rv(existing$journals %||% list())
    streak_rv(compute_streak(existing$journals %||% list()))
    output$login_msg <- renderUI(NULL)
    page("athlete")
  })

  # ── Position dropdown (signup) — updates when sport changes ──────────────
  observeEvent(input$su_sport, {
    sport     <- input$su_sport %||% "Basketball"
    positions <- sport_positions[[sport]] %||% sport_positions[["Other"]]
    updateSelectInput(session, "su_pos", choices = positions, selected = positions[1])
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # ── SIGN UP handler ───────────────────────────────────────────────────────
  observeEvent(input$btn_signup, {
    name  <- trimws(input$su_name  %||% "")
    sport <- input$su_sport %||% "Basketball"
    pos   <- trimws(input$su_pos   %||% "")
    level <- input$su_level %||% "High School"
    pin   <- trimws(input$su_pin   %||% "")

    if (!nzchar(name)) {
      output$signup_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;","Enter your name."))
      return()
    }
    if (!grepl("^[0-9]{4}$", pin)) {
      output$signup_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;",
        "PIN must be exactly 4 digits."))
      return()
    }
    if (!is.null(find_by_name_pin(name, pin))) {
      output$signup_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;",
        "An account with that name and PIN already exists — try logging in, or pick a different PIN."))
      return()
    }
    ath <- list(
      id       = new_id(),
      name     = name,
      pin      = pin,
      sport    = sport,
      position = pos,
      level    = level,
      games    = NULL,
      journals = list(),
      streak   = 0
    )
    save_athlete(ath)
    athlete_id(ath$id)
    games_rv(NULL)
    journals_rv(list())
    streak_rv(0)
    output$signup_msg <- renderUI(NULL)
    page("athlete")
  })

  # ── COACH LOGIN ───────────────────────────────────────────────────────────
  coach_reset_rv <- reactiveVal(FALSE)

  coach_login_screen <- function() {
    coach    <- get_coach()
    is_new   <- is.null(coach) || !nzchar(coach$pin %||% "")
    resetting <- isolate(coach_reset_rv())
    show_create <- is_new || resetting
    div(class = "login-wrap",
      div(class = "login-card",
        div(style = "text-align:center;margin-bottom:2rem;",
          div(class = "login-logo", "Stattrakker"),
          div(class = "login-sub", "Coach Access")),
        uiOutput("coach_login_msg"),
        div(style = "margin-bottom:1.5rem;",
          tags$label(class = "control-label",
            if (show_create) "Create your Coach PIN" else "Coach PIN"),
          if (show_create) div(style="font-size:12px;color:#6b7a99;margin-bottom:.5rem;",
            "Choose any 4-digit PIN — you'll use this every time you log in."),
          passwordInput("coach_pin_inp", NULL,
            placeholder = if (show_create) "Choose a 4-digit PIN" else "Enter your PIN")),
        actionButton("btn_coach_login",
          if (show_create) "Set PIN & Enter" else "Enter Dashboard",
          class = "btn-own",
          style = "width:100%;font-size:15px;padding:14px;margin-bottom:1rem;"),
        div(style = "text-align:center; margin-bottom:.75rem;",
          if (!show_create)
            actionLink("btn_reset_pin", "Forgot PIN? Reset it",
              style = "color:#5a6478;font-size:12px;font-weight:700;text-decoration:none;")),
        div(style = "text-align:center;",
          actionLink("go_athlete_login", "← Athlete login",
            style = "color:#5a6478;font-size:12px;font-weight:700;text-decoration:none;"))))
  }

  observeEvent(input$go_athlete_login, { coach_reset_rv(FALSE); page("login") })

  observeEvent(input$btn_reset_pin, {
    coach <- get_coach()
    if (!is.null(coach)) {
      coach$pin <- ""
      save_athlete(coach)
    }
    coach_reset_rv(TRUE)
    page("coach_login")
  })

  observeEvent(input$btn_coach_login, {
    pin   <- trimws(input$coach_pin_inp %||% "")
    coach <- get_coach()
    if (!grepl("^[0-9]{4}$", pin)) {
      output$coach_login_msg <- renderUI(div(class="alert-danger",
        style="margin-bottom:1rem;", "PIN must be exactly 4 digits."))
      return()
    }
    is_new <- is.null(coach) || !nzchar(coach$pin %||% "")
    if (is_new) {
      if (is.null(coach)) {
        save_athlete(list(id=new_id(), name="Coach", pin=pin, role="coach",
                          lineups=list(), schedule=list()))
      } else {
        coach$pin <- pin
        save_athlete(coach)
      }
      login_clear("coach")
      coach_reset_rv(FALSE)
      new_player_rv(NULL)
      page("coach")
    } else {
      locked <- login_locked_secs("coach")
      if (locked > 0) {
        output$coach_login_msg <- renderUI(div(class="alert-danger",
          style="margin-bottom:1rem;", lock_msg(locked)))
        return()
      }
      if (coach$pin != pin) {
        left    <- login_note_fail("coach")
        nowlock <- login_locked_secs("coach")
        msg  <- if (nowlock > 0) lock_msg(nowlock)
                else paste0("Wrong PIN. ", left, " attempt", if (left == 1) "" else "s",
                            " left before a temporary lockout.")
        output$coach_login_msg <- renderUI(div(class="alert-danger",
          style="margin-bottom:1rem;", msg))
        return()
      }
      login_clear("coach")
      coach_reset_rv(FALSE)
      new_player_rv(NULL)
      page("coach")
    }
  })

  # ── COACH SCREEN ─────────────────────────────────────────────────────────
  coach_screen <- function() {
    tagList(
      tags$nav(class = "navbar",
        div(class = "page-wrap",
          style = "display:flex;align-items:center;justify-content:space-between;padding-top:.75rem;padding-bottom:.75rem;",
          div(style = "display:flex;align-items:center;gap:.75rem;",
            div(class = "navbar-brand", "Stattrakker"),
            tags$span(class = "coach-badge", "COACH")),
          actionButton("btn_coach_logout", "Log out", class = "btn-ghost",
            style = "font-size:12px;padding:6px 14px;"))),
      div(class = "page-wrap",
        # ── Dashboard header ──
        div(class = "coach-header",
          div(class = "coach-date", format(Sys.Date(), "%A, %B %d, %Y")),
          div(class = "coach-title", "Coach Dashboard")),
        # ── Pill navigation ──
        uiOutput("coach_nav_pills"),
        # ── Active tab content ──
        uiOutput("coach_tab_content")
      )
    )
  }

  # Render pill nav with active state
  output$coach_nav_pills <- renderUI({
    active <- coach_tab_rv()
    pills  <- list(
      list(id = "roster",   icon = "🏆", label = "Roster"),
      list(id = "athletes", icon = "👤", label = "Athletes"),
      list(id = "lineups",  icon = "📋", label = "Lineups"),
      list(id = "schedule", icon = "📅", label = "Schedule"),
      list(id = "reports",  icon = "📊", label = "Reports"),
      list(id = "plan",     icon = "💳", label = "Plan")
    )
    div(class = "pill-nav",
      lapply(pills, function(p) {
        tags$button(
          class = paste0("pill", if (identical(active, p$id)) " active" else ""),
          onclick = sprintf(
            "Shiny.setInputValue('coach_tab','%s',{priority:'event'})", p$id),
          paste0(p$icon, "  ", p$label)
        )
      })
    )
  })

  # Route to correct tab
  output$coach_tab_content <- renderUI({
    switch(coach_tab_rv(),
      "roster"   = uiOutput("coach_roster_ui"),
      "athletes" = uiOutput("coach_athletes_ui"),
      "lineups"  = uiOutput("coach_lineups_ui"),
      "schedule" = uiOutput("coach_schedule_ui"),
      "reports"  = uiOutput("coach_reports_ui"),
      "plan"     = uiOutput("coach_plan_ui")
    )
  })

  observeEvent(input$coach_tab, {
    if (input$coach_tab == "roster") coach_sel_ath_rv(NULL)
    coach_tab_rv(input$coach_tab)
  })

  observeEvent(input$btn_coach_logout, {
    new_player_rv(NULL)
    coach_sel_ath_rv(NULL)
    coach_tab_rv("roster")
    page("login")
  })

  # ── COACH ROSTER TAB ──────────────────────────────────────────────────────
  output$coach_roster_ui <- renderUI({
    sel <- coach_sel_ath_rv()

    # ── ATHLETE PROFILE VIEW ─────────────────────────────────────────────────
    if (!is.null(sel)) {
      a <- get_athlete(sel)
      if (is.null(a)) { coach_sel_ath_rv(NULL); return(div("Athlete not found.")) }

      score   <- calc_passport(a$games, a$journals %||% list(), a$streak %||% 0)
      status  <- athlete_status(a)
      games_n <- tryCatch(if (!is.null(a$games) && is.data.frame(a$games)) nrow(a$games) else 0, error=function(e)0)
      jnl_n   <- length(a$journals %||% list())
      words   <- strsplit(trimws(a$name %||% "?"), "\\s+")[[1]]
      initials<- paste0(toupper(substring(words, 1, 1)), collapse="")

      # Score ring color
      ring_col <- if (score >= 80) "#C8F04B" else if (score >= 60) "#f59e0b" else "#f87171"

      div(style = "padding-top:1.5rem;",

        # ── Back button ──
        tags$button(class = "btn-ghost",
          style = "margin-bottom:1.5rem;font-size:13px;padding:8px 18px;cursor:pointer;",
          onclick = "Shiny.setInputValue('coach_back_roster', Math.random())",
          "← Back to Roster"),

        # ── Profile header ──
        div(style = "display:flex;align-items:center;gap:1.5rem;margin-bottom:2rem;flex-wrap:wrap;",
          # Avatar
          div(style = paste0("width:80px;height:80px;border-radius:50%;",
            "background:linear-gradient(135deg,", ring_col, ",", ring_col, "88);",
            "display:flex;align-items:center;justify-content:center;",
            "font-size:1.8rem;font-weight:900;color:#0a0c10;flex-shrink:0;",
            "border:3px solid ", ring_col, "44;"),
            initials),
          div(style = "flex:1;",
            div(style = "font-size:28px;font-weight:900;color:#f0f2f5;line-height:1;", a$name),
            div(style = "font-size:14px;color:#6b7a99;margin-top:.4rem;",
              paste0(a$sport %||% "—", " · ", a$position %||% "—", " · ", a$level %||% "—")),
            div(style = "margin-top:.6rem;display:flex;align-items:center;gap:.75rem;flex-wrap:wrap;",
              div(style = "background:#0f1f3a;border:1px solid #1e3a5f;border-radius:8px;padding:4px 14px;font-size:12px;font-weight:800;color:#60a5fa;letter-spacing:.2em;",
                paste0("PIN  ", a$pin %||% "—")),
              div(class = "status-badge",
                style = paste0("background:",status$bg,";border:1px solid ",status$border,";color:",status$color,";font-size:12px;padding:4px 14px;"),
                paste0(status$dot, " ", status$label))
            )
          )
        ),

        # ── Stat cards ──
        fluidRow(
          column(3, div(class = "stat-card",
            div(class="stat-label", "Passport"),
            div(class="stat-value", style=paste0("color:",ring_col,";"), score),
            div(class="stat-sub", "Overall score"))),
          column(3, div(class = "stat-card",
            div(class="stat-label", "Streak"),
            div(class="stat-value", style="color:#f59e0b;", paste0(a$streak %||% 0, " 🔥")),
            div(class="stat-sub", "Days in a row"))),
          column(3, div(class = "stat-card",
            div(class="stat-label", "Games Logged"),
            div(class="stat-value", style="color:#f0f2f5;", games_n),
            div(class="stat-sub", "Total games"))),
          column(3, div(class = "stat-card",
            div(class="stat-label", "Check-ins"),
            div(class="stat-value", style="color:#f0f2f5;", jnl_n),
            div(class="stat-sub", "Journal entries")))
        ),

        tags$hr(class = "own"),

        # ── Score breakdown ──
        div(class = "card", style = "padding:1.25rem;margin-bottom:1.5rem;",
          div(style="font-size:15px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;", "Passport Breakdown"),
          div(style="display:flex;flex-direction:column;gap:.6rem;",
            lapply(list(
              list("Game Performance", "55%", if (games_n > 0) {
                r <- suppressWarnings(as.numeric(a$games$Rating)); round(mean(r, na.rm=TRUE))
              } else 70, "#60a5fa"),
              list("Wellness Check-ins", "30%", if (jnl_n > 0) {
                round(mean(vapply(a$journals %||% list(), function(j) {
                  s <- suppressWarnings(as.numeric(j$score)[1L]); if (is.na(s)) 7 else s
                }, numeric(1)), na.rm=TRUE) * 10)
              } else 70, "#a78bfa"),
              list("Consistency Streak", "15%", min(100, (a$streak %||% 0)*10), "#f59e0b")
            ), function(row) {
              pct <- max(0, min(100, row[[3]]))
              div(style="display:flex;align-items:center;gap:.75rem;",
                div(style="width:130px;font-size:12px;color:#9ba8c0;font-weight:600;flex-shrink:0;", row[[1]]),
                div(style="width:32px;font-size:11px;color:#5a6478;flex-shrink:0;text-align:right;", row[[2]]),
                div(style="flex:1;background:#1a1f2b;border-radius:6px;height:10px;overflow:hidden;",
                  div(style=paste0("width:",pct,"%;height:100%;background:",row[[4]],";border-radius:6px;"))),
                div(style="width:36px;font-size:13px;font-weight:800;color:#f0f2f5;text-align:right;flex-shrink:0;", pct))
            })
          )
        ),

        # ── Games table ──
        if (games_n > 0) {
          df <- a$games
          div(class = "card", style = "padding:1.25rem;margin-bottom:1.5rem;",
            div(style="font-size:15px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;", "Recent Games"),
            div(style="overflow-x:auto;",
              tags$table(
                tags$thead(tags$tr(lapply(names(df), function(col)
                  tags$th(col, style="text-align:left;")))),
                tags$tbody(do.call(tagList, lapply(seq_len(nrow(df)), function(i)
                  tags$tr(lapply(names(df), function(col) tags$td(as.character(df[i, col]))))
                )))
              )
            )
          )
        } else {
          div(class = "alert-own", style="margin-bottom:1.5rem;",
            "No games logged yet — athlete needs to add their first game.")
        },

        # ── Journal entries ──
        div(class = "card", style = "padding:1.25rem;margin-bottom:1.5rem;",
          div(style="font-size:15px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;", "Journal Check-ins"),
          if (jnl_n == 0) {
            div(style="font-size:13px;color:#5a6478;", "No check-ins yet.")
          } else {
            do.call(tagList, lapply(rev(seq_along(a$journals)), function(i) {
              j <- a$journals[[i]]
              div(class = "journal-entry",
                div(style="display:flex;align-items:center;justify-content:space-between;",
                  div(class="je-date", j$date %||% "—"),
                  if (!is.null(j$score))
                    div(style="font-size:12px;font-weight:800;color:#C8F04B;",
                      paste0(j$score, "/10"))),
                div(class="je-note", j$note %||% "—")
              )
            }))
          }
        )
      )

    } else {
    # ── ROSTER LIST VIEW ─────────────────────────────────────────────────────
      athletes <- get_all_athletes()
      n        <- length(athletes)
      # Fixed: use vapply to guarantee numeric vector, not list
      scores   <- if (n > 0) vapply(athletes, function(a)
        tryCatch(calc_passport(a$games, a$journals %||% list(), a$streak %||% 0),
                 error = function(e) 0L), numeric(1)) else numeric(0)
      avg_sc   <- if (n == 0) 0 else round(mean(scores))
      active_n <- sum(vapply(athletes, function(a) {
        s <- tryCatch(as.numeric(a$streak %||% 0)[1L], error = function(e) 0)
        isTRUE(!is.na(s) && s >= 2)
      }, logical(1)))

      div(style = "padding-top:1.5rem;",

        # ── Team summary ──
        fluidRow(
          column(4, div(class = "stat-card",
            div(class="stat-label","Total Athletes"),
            div(class="stat-value", n),
            div(class="stat-sub","On roster"))),
          column(4, div(class = "stat-card",
            div(class="stat-label","Team Avg Passport"),
            div(class="stat-value", avg_sc),
            div(class="stat-sub","Overall score"))),
          column(4, div(class = "stat-card",
            div(class="stat-label","Active (streak ≥ 2)"),
            div(class="stat-value", active_n),
            div(class="stat-sub", paste0(n - active_n, " need attention"))))
        ),

        tags$hr(class = "own"),

        # ── Roster list ──
        div(style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:1.25rem;",
          div(style="font-size:18px;font-weight:800;color:#f0f2f5;", "Team Roster"),
          tags$button(class = "pill active",
            onclick = "Shiny.setInputValue('coach_tab','athletes',{priority:'event'})",
            "➕  Add Athlete")),

        if (n == 0) {
          div(class = "alert-own",
            "No athletes yet. Go to the ",
            tags$b("Athletes"), " tab to add your first player.")
        } else {
          do.call(tagList, lapply(seq_along(athletes), function(i) {
            a       <- athletes[[i]]
            score   <- scores[[i]]
            status  <- athlete_status(a)
            games_n <- tryCatch(if (!is.null(a$games) && is.data.frame(a$games)) nrow(a$games) else 0, error=function(e)0)
            jnl_n   <- length(a$journals %||% list())

            div(class = "athlete-row",
              style = "cursor:pointer;",
              div(style = "display:flex;align-items:center;gap:1rem;min-width:180px;",
                div(style = "font-size:1.5rem;", status$dot),
                div(
                  div(class = "ath-name", a$name),
                  div(class = "ath-sub",
                    paste0(a$sport %||% "—", " · ", a$position %||% "—", " · ", a$level %||% "—")))
              ),
              div(style = "display:flex;align-items:center;gap:1.5rem;flex-wrap:wrap;",
                div(style="text-align:center;",
                  div(class="ath-stat-val", score),
                  div(class="ath-stat-lbl","Passport")),
                div(style="text-align:center;",
                  div(class="ath-stat-val",style="color:#f0f2f5;",paste0(a$streak%||%0," 🔥")),
                  div(class="ath-stat-lbl","Streak")),
                div(style="text-align:center;",
                  div(class="ath-stat-val",style="color:#f0f2f5;",games_n),
                  div(class="ath-stat-lbl","Games")),
                div(style="text-align:center;",
                  div(class="ath-stat-val",style="color:#f0f2f5;",jnl_n),
                  div(class="ath-stat-lbl","Check-ins")),
                div(class = "status-badge",
                  style = paste0("background:",status$bg,";border:1px solid ",status$border,";color:",status$color,";"),
                  status$label),
                tags$button(class = "btn-ghost",
                  style = "font-size:12px;padding:6px 14px;cursor:pointer;",
                  onclick = sprintf(
                    "Shiny.setInputValue('coach_open_profile','%s',{priority:'event'})", a$id),
                  "View Profile →")
              )
            )
          }))
        }
      )
    }
  })

  observeEvent(input$btn_add_player, {
    name  <- trimws(input$new_ath_name  %||% "")
    sport <- input$new_ath_sport %||% "Basketball"
    pos   <- trimws(input$new_ath_pos   %||% "")
    level <- input$new_ath_level %||% "High School"
    if (!nzchar(name)) {
      output$add_player_msg <- renderUI(div(class="alert-danger",style="margin-bottom:1rem;",
        "Please enter the athlete's name."))
      return()
    }
    pin   <- gen_pin()
    tries <- 0
    while (!is.null(find_by_name_pin(name, pin)) && tries < 100) {
      pin <- gen_pin(); tries <- tries + 1
    }
    ath <- list(
      id       = new_id(),
      name     = name,
      pin      = pin,
      sport    = sport,
      position = pos,
      level    = level,
      games    = NULL,
      journals = list(),
      streak   = 0
    )
    save_athlete(ath)
    new_player_rv(list(name = name, pin = pin))
    output$add_player_msg <- renderUI(NULL)
    updateTextInput(session, "new_ath_name", value = "")
    # position dropdown stays on current sport — no reset needed
  })

  # ── Position dropdown (coach add athlete) — updates with sport ───────────
  observeEvent(input$new_ath_sport, {
    sport     <- input$new_ath_sport %||% "Basketball"
    positions <- sport_positions[[sport]] %||% sport_positions[["Other"]]
    updateSelectInput(session, "new_ath_pos", choices = positions, selected = positions[1])
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # ── ATHLETES TAB (add players + PIN management) ───────────────────────────
  output$coach_athletes_ui <- renderUI({
    athletes <- get_all_athletes()
    n        <- length(athletes)
    np       <- new_player_rv()

    div(style = "padding-top:1.5rem;",

      # ── PIN card (just added) ──
      if (!is.null(np))
        div(class = "pin-card",
          div(style="font-size:14px;color:#60a5fa;font-weight:800;margin-bottom:.3rem;",
            paste0("✅  ", np$name, " added!")),
          div(style="font-size:12px;color:#6b7a99;margin-bottom:.5rem;",
            "Give this PIN to the athlete — they use it to log in:"),
          div(class = "pin-display", np$pin),
          div(style="font-size:11px;color:#5a6478;",
            "Name + PIN is all they need. No email required.")),

      # ── Add player form ──
      div(class = "card", style = "padding:1.25rem;margin-bottom:1.75rem;",
        div(style="font-size:16px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;",
          "Add Player to Your Team"),
        uiOutput("add_player_msg"),
        fluidRow(
          column(6, textInput("new_ath_name", "Full Name",
            placeholder = "Athlete's full name")),
          column(6, selectInput("new_ath_sport", "Sport",
            choices = names(sport_positions)))
        ),
        fluidRow(
          column(6, selectInput("new_ath_pos", "Position",
            choices = sport_positions[["Basketball"]])),
          column(6, selectInput("new_ath_level", "Level",
            choices = c("Youth","Middle School","High School","College","Semi-Pro","Pro")))
        ),
        div(style="margin-top:1rem;",
          actionButton("btn_add_player", "Add Player + Generate PIN", class = "btn-own"))
      ),

      # ── Team PIN roster ──
      div(style = "font-size:18px;font-weight:800;color:#f0f2f5;margin-bottom:1.25rem;",
        paste0("Your Team  ·  ", n, " athlete", if (n != 1) "s" else "")),

      if (n == 0) {
        div(class = "alert-own",
          "No athletes yet. Add your first player above.")
      } else {
        do.call(tagList, lapply(athletes, function(a) {
          status <- athlete_status(a)
          div(class = "pin-roster-row",
            # Left: name + details
            div(style = "display:flex;align-items:center;gap:1rem;min-width:160px;",
              div(style = "font-size:1.4rem;", status$dot),
              div(
                div(class = "ath-name", a$name),
                div(class = "ath-sub",
                  paste0(a$sport %||% "—", " · ", a$position %||% "—", " · ", a$level %||% "—"))
              )
            ),
            # Right: PIN + status
            div(style = "display:flex;align-items:center;gap:1.25rem;flex-wrap:wrap;",
              div(
                div(style="font-size:10px;text-transform:uppercase;letter-spacing:.08em;color:#6b7a99;font-weight:700;margin-bottom:2px;",
                  "Login PIN"),
                div(class = "pin-big", a$pin %||% "—")
              ),
              div(class = "status-badge",
                style = paste0("background:",status$bg,";border:1px solid ",status$border,";color:",status$color,";"),
                status$label)
            )
          )
        }))
      }
    )
  })

  # ── COACH PROFILE: open / close ───────────────────────────────────────────
  observeEvent(input$coach_open_profile, {
    coach_sel_ath_rv(input$coach_open_profile)
  })

  observeEvent(input$coach_back_roster, {
    coach_sel_ath_rv(NULL)
  })

  # ── COACH LINEUPS TAB ─────────────────────────────────────────────────────
  output$coach_lineups_ui <- renderUI({
    coach   <- get_coach()
    lineups <- if (!is.null(coach)) coach$lineups %||% list() else list()

    div(style = "padding-top:1.5rem;",

      # ── Create lineup form ──
      div(class = "card", style = "padding:1.25rem;margin-bottom:1.5rem;",
        div(style="font-size:16px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;",
          "Create Lineup"),
        fluidRow(
          column(6, textInput("lineup_name", "Lineup Name",
            placeholder = "e.g. Starters, Defense, Offense")),
          column(6, selectInput("lineup_sport", "Sport",
            choices = names(sport_positions)))
        ),
        div(style = "margin-top:1rem;",
          div(style="font-size:12px;color:#9ba8c0;font-weight:600;margin-bottom:.75rem;",
            "Assign players to each position:"),
          uiOutput("lineup_slots")
        ),
        div(style = "margin-top:1.25rem;",
          actionButton("btn_save_lineup", "Save Lineup", class = "btn-own"),
          uiOutput("lineup_msg")
        )
      ),

      # ── Saved lineups ──
      div(style="font-size:18px;font-weight:800;color:#f0f2f5;margin-bottom:1.25rem;",
        "Saved Lineups"),

      if (length(lineups) == 0) {
        div(class = "alert-own", "No lineups saved yet. Create one above.")
      } else {
        do.call(tagList, lapply(names(lineups), function(lname) {
          l       <- lineups[[lname]]
          slots_l <- l$slots %||% list()

          div(class = "lineup-card",
            div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:1rem;",
              div(style="font-size:16px;font-weight:800;color:#f0f2f5;", lname),
              div(style="font-size:12px;color:#6b7a99;font-weight:600;", l$sport %||% "")
            ),
            div(style="display:grid;grid-template-columns:repeat(auto-fill,minmax(160px,1fr));gap:.5rem;",
              lapply(slots_l, function(slot) {
                pid    <- slot$player_id %||% ""
                pname  <- if (nzchar(pid)) {
                  p <- get_athlete(pid)
                  if (!is.null(p)) p$name else "(removed)"
                } else "(empty)"
                div(class = "pos-slot",
                  div(class = "pos-label", slot$position %||% ""),
                  if (nzchar(pid) && pname != "(empty)" && pname != "(removed)")
                    div(class = "pos-player", pname)
                  else
                    div(class = "pos-empty", pname)
                )
              })
            )
          )
        }))
      }
    )
  })

  output$lineup_slots <- renderUI({
    sport     <- input$lineup_sport %||% "Basketball"
    positions <- sport_positions[[sport]] %||% sport_positions[["Other"]]
    athletes  <- get_all_athletes()
    choices   <- c("(empty)" = "")
    if (length(athletes) > 0)
      choices <- c("(empty)" = "",
        setNames(sapply(athletes, `[[`, "id"), sapply(athletes, `[[`, "name")))

    div(style="display:grid;grid-template-columns:repeat(auto-fill,minmax(210px,1fr));gap:.75rem;",
      lapply(seq_along(positions), function(i) {
        div(
          tags$label(class="control-label",style="font-size:11px;margin-bottom:3px;",
            positions[[i]]),
          selectInput(paste0("lineup_slot_", i), NULL,
            choices = choices, width = "100%")
        )
      })
    )
  })

  observeEvent(input$btn_save_lineup, {
    lname <- trimws(input$lineup_name  %||% "")
    sport <- input$lineup_sport %||% "Basketball"
    if (!nzchar(lname)) {
      output$lineup_msg <- renderUI(div(class="alert-danger",style="margin-top:.75rem;",
        "Enter a lineup name."))
      return()
    }
    positions <- sport_positions[[sport]] %||% sport_positions[["Other"]]
    slots <- lapply(seq_along(positions), function(i) {
      list(position  = positions[[i]],
           player_id = input[[paste0("lineup_slot_", i)]] %||% "")
    })
    coach <- get_coach()
    if (is.null(coach)) return()
    lineups        <- coach$lineups %||% list()
    lineups[[lname]] <- list(name = lname, sport = sport, slots = slots)
    coach$lineups  <- lineups
    save_athlete(coach)
    updateTextInput(session, "lineup_name", value = "")
    output$lineup_msg <- renderUI(div(class="alert-success",style="margin-top:.75rem;",
      paste0("Lineup '", lname, "' saved!")))
  })

  # ── COACH SCHEDULE TAB ────────────────────────────────────────────────────
  output$coach_schedule_ui <- renderUI({
    coach    <- get_coach()
    schedule <- if (!is.null(coach)) coach$schedule %||% list() else list()

    cur_month <- sched_month_rv()
    yr  <- as.integer(substr(cur_month, 1, 4))
    mo  <- as.integer(substr(cur_month, 6, 7))
    first_day <- as.Date(sprintf("%04d-%02d-01", yr, mo))
    last_day  <- seq(first_day, by="month", length.out=2)[2] - 1
    n_days    <- as.integer(last_day - first_day + 1)
    start_dow <- as.integer(format(first_day, "%w"))  # 0=Sun
    today     <- Sys.Date()
    month_lbl <- format(first_day, "%B %Y")

    # Get all sports on team
    athletes    <- get_all_athletes()
    team_sports <- unique(c("All Sports", unlist(lapply(athletes, function(a) a$sport %||% NULL)), names(sport_positions)))

    # Index games by day for this month
    games_by_day <- list()
    for (g in schedule) {
      d <- g$date %||% ""
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", d) && substr(d, 1, 7) == cur_month) {
        dn <- as.character(as.integer(substr(d, 9, 10)))
        games_by_day[[dn]] <- c(games_by_day[[dn]], list(g))
      }
    }

    # Build calendar cells
    total_cells <- start_dow + n_days
    n_rows <- ceiling(total_cells / 7)
    padded <- n_rows * 7

    cells <- lapply(seq_len(padded), function(idx) {
      day_n <- idx - start_dow
      if (day_n < 1 || day_n > n_days) return(div(class="cal-cell empty"))
      day_games <- games_by_day[[as.character(day_n)]]
      is_today  <- (first_day + day_n - 1) == today
      div(class = paste0("cal-cell", if(is_today) " cal-today" else ""),
        div(class="cal-day-num", day_n),
        if (!is.null(day_games))
          do.call(tagList, lapply(day_games, function(g) {
            sp  <- g$sport %||% ""
            ha  <- g$home_away %||% "Home"
            col <- if (ha=="Home") "#C8F04B" else if (ha=="Away") "#f87171" else "#60a5fa"
            div(class="cal-event",
              style=paste0("background:",col,"18;border-left:3px solid ",col,";"),
              div(class="cal-event-title", style=paste0("color:",col,";"),
                paste0(if(nzchar(sp)) paste0(sp," · "), "vs ", g$opponent %||% "?")),
              if (nzchar(g$location %||% ""))
                div(class="cal-event-sub", g$location)
            )
          }))
      )
    })

    rows_ui <- lapply(seq_len(n_rows), function(r) {
      div(class="cal-row",
        do.call(tagList, cells[((r-1)*7+1):(r*7)])
      )
    })

    # Games not in current month view (old text-format or other months)
    other <- Filter(function(g) {
      d <- g$date %||% ""
      !(grepl("^\\d{4}-\\d{2}-\\d{2}$", d) && substr(d, 1, 7) == cur_month)
    }, schedule)

    div(style="padding-top:1.5rem;",

      # ── Add game form ──
      div(class="card", style="margin-bottom:1.5rem;",
        div(style="font-size:16px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;", "Schedule a Game"),
        fluidRow(
          column(3, dateInput("sched_date", "Date", value=Sys.Date(), format="M d, yyyy")),
          column(3, textInput("sched_opponent", "Opponent", placeholder="vs. Westview")),
          column(3, selectInput("sched_sport", "Sport", choices=team_sports)),
          column(3, selectInput("sched_ha", "Home/Away", choices=c("Home","Away","Neutral")))
        ),
        fluidRow(
          column(6, textInput("sched_location", "Location", placeholder="Arena / Field name")),
          column(6, textInput("sched_notes", "Notes", placeholder="Optional — jersey color, carpool…"))
        ),
        actionButton("btn_add_sched_game", "Add to Calendar", class="btn-own"),
        uiOutput("sched_msg")
      ),

      # ── Calendar nav ──
      div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:.75rem;",
        tags$button(class="pill",
          onclick="Shiny.setInputValue('sched_month_nav','prev',{priority:'event'})", "‹ Prev"),
        div(style="font-size:18px;font-weight:800;color:#f0f2f5;", month_lbl),
        tags$button(class="pill",
          onclick="Shiny.setInputValue('sched_month_nav','next',{priority:'event'})", "Next ›")
      ),

      # ── Calendar grid ──
      div(class="cal-grid",
        div(class="cal-header-row",
          lapply(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), function(d)
            div(class="cal-head-cell", d))
        ),
        do.call(tagList, rows_ui)
      ),

      # ── Other games (not on this month's calendar) ──
      if (length(other) > 0)
        div(
          div(style="font-size:13px;font-weight:800;color:#6b7a99;text-transform:uppercase;letter-spacing:.07em;margin-bottom:.75rem;",
            "Other Scheduled Games"),
          do.call(tagList, lapply(other, function(g) {
            ha    <- g$home_away %||% "Home"
            ha_col <- if (ha=="Home") "#C8F04B" else if (ha=="Away") "#f87171" else "#60a5fa"
            div(class="sched-row",
              div(style="flex:1;",
                div(class="ath-name",
                  paste0(if(nzchar(g$sport %||% "")) paste0("[",g$sport,"] ") else "", g$opponent %||% "—")),
                div(class="ath-sub", paste0(g$date %||% "TBD", " · ", g$location %||% "TBD"))
              ),
              div(class="status-badge", style=paste0("color:",ha_col,";"), ha)
            )
          }))
        )
    )
  })

  observeEvent(input$sched_month_nav, {
    cur <- sched_month_rv()
    yr  <- as.integer(substr(cur, 1, 4))
    mo  <- as.integer(substr(cur, 6, 7))
    if (input$sched_month_nav == "prev") {
      mo <- mo - 1; if (mo < 1)  { mo <- 12; yr <- yr - 1 }
    } else {
      mo <- mo + 1; if (mo > 12) { mo <- 1;  yr <- yr + 1 }
    }
    sched_month_rv(sprintf("%04d-%02d", yr, mo))
  })

  observeEvent(input$btn_add_sched_game, {
    opp  <- trimws(input$sched_opponent %||% "")
    date <- as.character(input$sched_date %||% Sys.Date())
    if (!nzchar(opp)) {
      output$sched_msg <- renderUI(div(class="alert-danger", style="margin-top:.75rem;",
        "Opponent name is required."))
      return()
    }
    game <- list(
      opponent  = opp,
      date      = date,
      sport     = input$sched_sport    %||% "All Sports",
      home_away = input$sched_ha       %||% "Home",
      location  = trimws(input$sched_location %||% ""),
      notes     = trimws(input$sched_notes    %||% "")
    )
    coach <- get_coach()
    if (is.null(coach)) return()
    coach$schedule <- c(coach$schedule %||% list(), list(game))
    save_athlete(coach)
    updateTextInput(session, "sched_opponent", value="")
    updateTextInput(session, "sched_location", value="")
    updateTextInput(session, "sched_notes",    value="")
    output$sched_msg <- renderUI(div(class="alert-success", style="margin-top:.75rem;",
      paste0(opp, " added to calendar!")))
  })

  # ── COACH REPORTS TAB ────────────────────────────────────────────────────
  output$coach_reports_ui <- renderUI({
    athletes <- get_all_athletes()
    n        <- length(athletes)

    if (n == 0) return(div(style="padding:3rem;text-align:center;color:#6b7a99;",
      "No athletes yet. Add players in the Athletes tab."))

    # Build stats for each athlete
    stats <- lapply(athletes, function(a) {
      score   <- tryCatch(calc_passport(a$games, a$journals %||% list(), a$streak %||% 0), error=function(e) 0)
      games_n <- tryCatch(if (is.null(a$games) || !is.data.frame(a$games)) 0L else nrow(a$games), error=function(e) 0L)
      wins    <- tryCatch(if (games_n == 0) 0L else sum(grepl("^W", a$games$Result), na.rm=TRUE), error=function(e) 0L)
      streak  <- tryCatch(as.integer(as.numeric(a$streak %||% 0)[1L]), error=function(e) 0L)
      jcount  <- tryCatch(length(a$journals %||% list()), error=function(e) 0L)
      list(name=a$name, sport=a$sport %||% "—", score=score,
           games=games_n, wins=wins, streak=streak, journals=jcount)
    })

    # Sort by score descending for leaderboard
    stats <- stats[order(sapply(stats, `[[`, "score"), decreasing=TRUE)]

    # Score bar helper
    score_bar <- function(s) {
      col <- if (s >= 80) "#C8F04B" else if (s >= 60) "#f0c040" else "#e05555"
      div(style="display:flex;align-items:center;gap:.75rem;",
        div(style=sprintf("flex:1;height:8px;border-radius:99px;background:#1e2330;overflow:hidden;"),
          div(style=sprintf("width:%s%%;height:100%%;background:%s;border-radius:99px;transition:width .4s;", s, col))),
        div(style=sprintf("font-size:14px;font-weight:900;color:%s;min-width:36px;text-align:right;", col), s)
      )
    }

    # Team summary cards
    avg_score  <- round(mean(sapply(stats, `[[`, "score")))
    total_games <- sum(sapply(stats, `[[`, "games"))
    active_streaks <- sum(sapply(stats, function(s) s$streak >= 2))

    div(style="padding-top:1.5rem;",
      # ── Summary row ──
      div(style="display:flex;gap:1rem;margin-bottom:1.5rem;flex-wrap:wrap;",
        div(class="card", style="flex:1;min-width:120px;text-align:center;padding:1.25rem;",
          div(style="font-size:28px;font-weight:900;color:#C8F04B;", avg_score),
          div(style="font-size:11px;color:#6b7a99;font-weight:700;text-transform:uppercase;letter-spacing:.07em;margin-top:.25rem;", "Avg Score")),
        div(class="card", style="flex:1;min-width:120px;text-align:center;padding:1.25rem;",
          div(style="font-size:28px;font-weight:900;color:#C8F04B;", total_games),
          div(style="font-size:11px;color:#6b7a99;font-weight:700;text-transform:uppercase;letter-spacing:.07em;margin-top:.25rem;", "Games Logged")),
        div(class="card", style="flex:1;min-width:120px;text-align:center;padding:1.25rem;",
          div(style="font-size:28px;font-weight:900;color:#C8F04B;", active_streaks),
          div(style="font-size:11px;color:#6b7a99;font-weight:700;text-transform:uppercase;letter-spacing:.07em;margin-top:.25rem;", "Active Streaks"))
      ),

      # ── Leaderboard ──
      div(class="card", style="margin-bottom:1.5rem;",
        div(style="font-size:13px;font-weight:900;color:#9ba8c0;text-transform:uppercase;letter-spacing:.08em;margin-bottom:1rem;", "📊 Leaderboard"),
        do.call(tagList, lapply(seq_along(stats), function(i) {
          s <- stats[[i]]
          medal <- if (i == 1) "🥇" else if (i == 2) "🥈" else if (i == 3) "🥉" else paste0("#", i)
          div(style="margin-bottom:1rem;",
            div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:.4rem;",
              div(style="display:flex;align-items:center;gap:.6rem;",
                div(style="font-size:16px;", medal),
                div(style="font-weight:700;color:#f0f2f5;font-size:14px;", s$name),
                div(style="font-size:11px;color:#6b7a99;", s$sport)
              ),
              div(style="font-size:12px;color:#6b7a99;",
                paste0(s$games, " games · ", s$wins, "W · ", s$streak, " streak"))
            ),
            score_bar(s$score)
          )
        }))
      ),

      # ── Detail cards ──
      div(style="font-size:13px;font-weight:900;color:#9ba8c0;text-transform:uppercase;letter-spacing:.08em;margin-bottom:.75rem;", "Individual Reports"),
      do.call(tagList, lapply(stats, function(s) {
        div(class="card", style="margin-bottom:.75rem;",
          div(style="display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:.75rem;",
            div(
              div(style="font-weight:800;color:#f0f2f5;font-size:15px;", s$name),
              div(style="font-size:12px;color:#6b7a99;margin-top:.2rem;", s$sport)
            ),
            div(style="text-align:right;",
              div(style="font-size:11px;color:#6b7a99;", "Check-ins"),
              div(style="font-size:18px;font-weight:900;color:#C8F04B;", s$journals))
          ),
          score_bar(s$score),
          div(style="display:flex;gap:1.5rem;margin-top:.75rem;",
            div(style="font-size:12px;color:#6b7a99;", tags$b(style="color:#f0f2f5;", s$games), " games"),
            div(style="font-size:12px;color:#6b7a99;", tags$b(style="color:#f0f2f5;", s$wins), " wins"),
            div(style="font-size:12px;color:#6b7a99;", tags$b(style="color:#f0f2f5;", s$streak), " day streak")
          )
        )
      }))
    )
  })

  # ── COACH PLAN TAB ───────────────────────────────────────────────────────
  output$coach_plan_ui <- renderUI({
    athletes  <- get_all_athletes()
    n         <- length(athletes)
    team_price   <- 2.99   # per athlete/month
    indiv_price  <- 4.99   # per individual/month
    monthly   <- n * team_price

    div(style = "padding-top:1.5rem;",

      # ── Current Plan card ──
      div(class = "card", style = "padding:1.5rem;margin-bottom:1.5rem;background:linear-gradient(135deg,#1a2540 0%,#0f1726 100%);border:1.5px solid #f59e0b44;",
        div(style = "display:flex;align-items:flex-start;justify-content:space-between;",
          div(
            div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.1em;color:#9ba8c0;margin-bottom:.35rem;", "Current Plan"),
            div(style = "font-size:2rem;font-weight:900;color:#f59e0b;line-height:1;", "Unlimited"),
            div(style = "margin-top:.4rem;font-size:13px;color:#9ba8c0;", "Beta access · No billing until launch")
          ),
          div(style = "background:#f59e0b22;border:1px solid #f59e0b55;border-radius:9999px;padding:6px 16px;font-size:12px;font-weight:700;color:#f59e0b;white-space:nowrap;",
            "BETA")
        ),
        tags$hr(style = "border:none;border-top:1px solid #ffffff10;margin:1.25rem 0;"),
        div(style = "display:flex;gap:2.5rem;",
          div(
            div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:#6b7a99;", "Athletes on Roster"),
            div(style = "font-size:1.75rem;font-weight:800;color:#f0f2f5;margin-top:.15rem;", n),
            div(style = "font-size:11px;color:#6b7a99;margin-top:.1rem;", "Unlimited seats right now")
          ),
          div(style = "width:1px;background:#ffffff10;"),
          div(
            div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:#6b7a99;", "Seat Limit"),
            div(style = "font-size:1.75rem;font-weight:800;color:#22c55e;margin-top:.15rem;", "∞"),
            div(style = "font-size:11px;color:#6b7a99;margin-top:.1rem;", "No restrictions")
          )
        )
      ),

      # ── Pricing preview card ──
      div(class = "card", style = "padding:1.25rem;margin-bottom:1.5rem;",
        div(style = "font-size:15px;font-weight:800;color:#f0f2f5;margin-bottom:.25rem;", "When Billing Launches"),
        div(style = "font-size:12px;color:#6b7a99;margin-bottom:1.25rem;",
          "Here's what your plan would cost based on today's roster — no charge until we flip the switch."),

        div(style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(200px,1fr));gap:1rem;margin-bottom:1.25rem;",
          div(style = "background:#0f1726;border:1px solid #1e2d4a;border-radius:10px;padding:1rem;",
            div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:#6b7a99;", "Price Per Athlete"),
            div(style = "font-size:1.6rem;font-weight:800;color:#60a5fa;margin-top:.25rem;",
              paste0("$", format(team_price, nsmall = 2))),
            div(style = "font-size:11px;color:#6b7a99;", "per month")
          ),
          div(style = "background:#0f1726;border:1px solid #1e2d4a;border-radius:10px;padding:1rem;",
            div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:#6b7a99;", "Your Roster"),
            div(style = "font-size:1.6rem;font-weight:800;color:#60a5fa;margin-top:.25rem;", n),
            div(style = "font-size:11px;color:#6b7a99;", "athletes")
          ),
          div(style = "background:#0f1726;border:1px solid #f59e0b33;border-radius:10px;padding:1rem;",
            div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:#6b7a99;", "Est. Monthly"),
            div(style = "font-size:1.6rem;font-weight:800;color:#f59e0b;margin-top:.25rem;",
              if (n == 0) "$0" else paste0("$", format(monthly, big.mark=",", nsmall=2))),
            div(style = "font-size:11px;color:#6b7a99;", "projected")
          )
        ),

        div(style = "background:#f59e0b0d;border:1px solid #f59e0b22;border-radius:8px;padding:.75rem 1rem;display:flex;align-items:center;gap:.6rem;",
          tags$span(style="font-size:16px;","💡"),
          div(style = "font-size:12px;color:#9ba8c0;line-height:1.5;",
            "Pricing is not final and you won't be charged during beta. ",
            tags$b(style="color:#f0f2f5;","Add as many athletes as you need right now.")))
      ),

      # ── What's included ──
      div(class = "card", style = "padding:1.25rem;",
        div(style = "font-size:15px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;", "What's Included"),
        div(style = "display:flex;flex-direction:column;gap:.6rem;",
          lapply(list(
            list("✅", "Unlimited roster management + PIN-based athlete logins"),
            list("✅", "Athlete Passport scores — engagement, games, check-ins"),
            list("✅", "Sport-specific lineup builder for every sport"),
            list("✅", "Season schedule with opponent, date, location + notes"),
            list("✅", "Team dashboard — avg passport, streaks, attention alerts"),
            list("🔜", "Athlete performance trends over time"),
            list("🔜", "Parent/guardian notifications"),
            list("🔜", "CSV roster import")
          ), function(row) {
            div(style = "display:flex;align-items:center;gap:.65rem;font-size:13px;color:#c8d0e0;",
              tags$span(style="font-size:14px;", row[[1]]),
              row[[2]])
          })
        )
      )
    )
  })

  # ── ATHLETE SCREEN ────────────────────────────────────────────────────────
  athlete_screen <- function() {
    ath <- athlete(); req(ath)
    tagList(
      tags$nav(class = "navbar",
        div(class = "page-wrap",
          style = "display:flex;align-items:center;justify-content:space-between;padding-top:.75rem;padding-bottom:.75rem;",
          div(class = "navbar-brand", "Stattrakker"),
          div(style = "font-size:13px;font-weight:700;color:#9ba8c0;", ath$name),
          actionButton("btn_logout", "Log out", class = "btn-ghost",
            style = "font-size:12px;padding:6px 14px;"))),
      div(class = "page-wrap",
        uiOutput("athlete_nav_pills"),
        uiOutput("athlete_tab_content")
      )
    )
  }

  # Pill-style tabs for the athlete view (matches the coach dashboard look)
  output$athlete_nav_pills <- renderUI({
    active <- athlete_tab_rv()
    pills  <- list(
      list(id = "passport", icon = "🪪", label = "Passport"),
      list(id = "games",    icon = "🏀", label = "Games"),
      list(id = "journal",  icon = "📓", label = "Journal"),
      list(id = "settings", icon = "⚙️", label = "Settings")
    )
    div(class = "pill-nav",
      lapply(pills, function(p) {
        tags$button(
          class = paste0("pill", if (identical(active, p$id)) " active" else ""),
          onclick = sprintf(
            "Shiny.setInputValue('athlete_tab','%s',{priority:'event'})", p$id),
          paste0(p$icon, "  ", p$label)
        )
      })
    )
  })

  output$athlete_tab_content <- renderUI({
    switch(athlete_tab_rv(),
      "games"    = uiOutput("games_tab"),
      "journal"  = uiOutput("journal_tab"),
      "settings" = uiOutput("settings_tab"),
      uiOutput("passport_tab")
    )
  })

  observeEvent(input$athlete_tab, { athlete_tab_rv(input$athlete_tab) })

  observe({
    ath <- athlete(); req(ath)
    ath$games    <- games_rv()
    ath$journals <- journals_rv()
    ath$streak   <- streak_rv()
    save_athlete(ath)
  })

  observeEvent(input$btn_logout, {
    athlete_id(NULL)
    page("login")
  })

  # ── PASSPORT TAB ──────────────────────────────────────────────────────────
  output$passport_tab <- renderUI({
    ath   <- athlete(); req(ath)
    score <- passport()
    g     <- games_rv()
    avg_r <- if (is.null(g) || !is.data.frame(g) || nrow(g)==0) "—"
             else round(mean(suppressWarnings(as.numeric(g$Rating)), na.rm=TRUE))
    div(style = "padding-top:1.5rem;",
      div(class = "passport-hero", style = "margin-bottom:1.5rem;",
        div(style = "display:flex;align-items:center;gap:2rem;flex-wrap:wrap;",
          div(
            div(class = "passport-score", score),
            div(style = "font-size:11px;color:#6b7a99;font-weight:700;text-transform:uppercase;letter-spacing:.08em;margin-top:6px;",
              "Passport Score")),
          div(
            div(style = "font-size:18px;font-weight:800;color:#f0f2f5;", ath$name),
            div(style = "font-size:13px;color:#6b7a99;margin-top:3px;",
              paste0(ath$sport %||% "—", " · ", ath$position %||% "—", " · ", ath$level %||% "—"))))),
      fluidRow(
        column(4, div(class="stat-card",
          div(class="stat-label","Games Played"),
          div(class="stat-value", if (is.null(g)||!is.data.frame(g)) 0 else nrow(g)),
          div(class="stat-sub","This season"))),
        column(4, div(class="stat-card",
          div(class="stat-label","Avg Rating"),
          div(class="stat-value", avg_r),
          div(class="stat-sub","Performance"))),
        column(4, div(class="stat-card",
          div(class="stat-label","Streak"),
          div(class="stat-value", paste0(streak_rv(), " 🔥")),
          div(class="stat-sub","Days checked in"))))
    )
  })

  # ── GAMES TAB ─────────────────────────────────────────────────────────────
  output$games_tab <- renderUI({
    div(style = "padding-top:1.5rem;",
      div(class = "card", style = "padding:1.25rem;margin-bottom:1.5rem;",
        div(style="font-size:16px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;","Log a Game"),
        fluidRow(
          column(4, textInput("g_date",     "Date",     placeholder="Jun 18")),
          column(4, textInput("g_opponent", "Opponent", placeholder="vs. Westview")),
          column(4, textInput("g_result",   "Result",   placeholder="W 72-61"))),
        uiOutput("sport_stat_inputs"),
        fluidRow(
          column(6,
            tags$label(class="control-label","Rating (0–100)"),
            sliderInput("g_rating", NULL, min=0, max=100, value=75)),
          column(6, style="padding-top:1.75rem;",
            actionButton("btn_add_game","Add Game",class="btn-own"))),
        uiOutput("game_msg")),
      tableOutput("games_table"))
  })

  output$sport_stat_inputs <- renderUI({
    ath   <- athlete()
    sport <- ath$sport %||% "Other"
    stats <- sport_stats[[sport]] %||% sport_stats[["Other"]]
    cols  <- if (length(stats) <= 2) 6 else 4
    tagList(
      div(style="font-size:11px;font-weight:700;color:#6b7a99;text-transform:uppercase;letter-spacing:.06em;margin:1rem 0 .5rem;",
        paste(sport, "Stats")),
      fluidRow(
        lapply(stats, function(s) {
          inp_id <- paste0("gs_", gsub("[^a-zA-Z0-9]", "_", s))
          column(cols, numericInput(inp_id, s, value=NA, min=0, step=1))
        })
      )
    )
  })

  observeEvent(input$btn_add_game, {
    date <- trimws(input$g_date     %||% "")
    opp  <- trimws(input$g_opponent %||% "")
    if (!nzchar(date) || !nzchar(opp)) {
      output$game_msg <- renderUI(div(class="alert-danger",style="margin-top:.75rem;","Date and opponent required."))
      return()
    }
    ath   <- isolate(athlete())
    sport <- ath$sport %||% "Other"
    stats <- sport_stats[[sport]] %||% sport_stats[["Other"]]
    new_row <- data.frame(Date=date, Opponent=opp,
      Result=trimws(input$g_result %||% ""), Rating=input$g_rating, stringsAsFactors=FALSE)
    for (s in stats) {
      inp_id <- paste0("gs_", gsub("[^a-zA-Z0-9]", "_", s))
      val <- input[[inp_id]]
      new_row[[s]] <- if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
    }
    g <- games_rv()
    if (!is.null(g) && is.data.frame(g) && nrow(g) > 0) {
      for (col in setdiff(names(new_row), names(g))) g[[col]] <- NA_real_
      for (col in setdiff(names(g), names(new_row))) new_row[[col]] <- NA_real_
      games_rv(rbind(g, new_row))
    } else {
      games_rv(new_row)
    }
    output$game_msg <- renderUI(div(class="alert-success",style="margin-top:.75rem;","Game logged!"))
  })

  output$games_table <- renderTable({
    g <- games_rv()
    if (is.null(g) || !is.data.frame(g) || nrow(g)==0)
      return(data.frame(Message="No games logged yet."))
    g
  }, striped=TRUE, hover=TRUE, bordered=TRUE)

  # ── JOURNAL TAB ───────────────────────────────────────────────────────────
  output$journal_tab <- renderUI({
    journals <- journals_rv()
    div(style = "padding-top:1.5rem;",
      div(class = "card", style = "padding:1.25rem;margin-bottom:1.5rem;",
        div(style="font-size:16px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;","Today's Check-in"),
        fluidRow(
          column(6,
            tags$label(class="control-label","Wellness Score (1–10)"),
            sliderInput("j_score",NULL,min=1,max=10,value=7)),
          column(6,
            selectInput("j_mood","Mood",
              choices=c("🔥 Locked in"="🔥","💪 Strong"="💪","😐 Neutral"="😐",
                        "😴 Low energy"="😴","🤕 Hurt/sore"="🤕")))),
        textAreaInput("j_note","Note (private)",placeholder="How are you feeling?",rows=3),
        actionButton("btn_add_journal","Save Check-in",class="btn-own"),
        uiOutput("journal_msg")),
      if (length(journals))
        do.call(tagList, lapply(rev(journals), function(j) {
          div(class="journal-entry",
            div(class="je-date",paste0(j$date,"  ·  Score: ",j$score,"/10  ·  ",j$mood)),
            div(class="je-note",if (nzchar(j$note%||%"")) j$note else "(no note)"))
        }))
      else
        div(class="alert-own","No journal entries yet. Log your first check-in above.")
    )
  })

  observeEvent(input$btn_add_journal, {
    entry <- list(date=format(Sys.Date(),"%b %d"),
                  iso=as.character(Sys.Date()),
                  mood=strsplit(input$j_mood," ")[[1]][1],
                  score=input$j_score,
                  note=trimws(input$j_note%||%""))
    js <- c(journals_rv(), list(entry))
    journals_rv(js)
    streak_rv(compute_streak(js))
    updateTextAreaInput(session,"j_note",value="")
    output$journal_msg <- renderUI(div(class="alert-success",style="margin-top:.75rem;","Check-in saved!"))
  })

  # ── SETTINGS TAB ──────────────────────────────────────────────────────────
  output$settings_tab <- renderUI({
    ath <- athlete(); req(ath)
    div(style = "padding-top:1.5rem;",
      div(class="card",style="padding:1.25rem;max-width:500px;",
        div(style="font-size:16px;font-weight:800;color:#f0f2f5;margin-bottom:1rem;","Profile Settings"),
        textInput("set_name","Name",   value=ath$name),
        selectInput("set_sport","Sport",
          choices=names(sport_positions),
          selected=ath$sport),
        selectInput("set_pos","Position",
          choices = sport_positions[[ath$sport %||% "Basketball"]],
          selected = ath$position %||% ""),
        selectInput("set_level","Level",
          choices=c("Youth","Middle School","High School","College","Semi-Pro","Pro"),
          selected=ath$level%||%"High School"),
        actionButton("btn_save_settings","Save",class="btn-own"),
        uiOutput("settings_msg")))
  })

  # ── Position dropdown (settings) — updates with sport ────────────────────
  observeEvent(input$set_sport, {
    sport     <- input$set_sport %||% "Basketball"
    positions <- sport_positions[[sport]] %||% sport_positions[["Other"]]
    updateSelectInput(session, "set_pos", choices = positions)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$btn_save_settings, {
    ath <- athlete(); req(ath)
    ath$name     <- trimws(input$set_name %||% ath$name)
    ath$sport    <- input$set_sport
    ath$position <- trimws(input$set_pos  %||% "")
    ath$level    <- input$set_level
    save_athlete(ath)
    output$settings_msg <- renderUI(div(class="alert-success",style="margin-top:.75rem;","Saved."))
  })

} # end server

shinyApp(ui, server)
