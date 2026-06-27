#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Publish the Stattrakker landing page at / and keep the Shiny app at /own-it/.
# Safe: backs up nginx config, validates with `nginx -t`, auto-reverts on failure.
# ─────────────────────────────────────────────────────────────────────────────
set -u
TS=$(date +%Y%m%d-%H%M%S)
RAW="https://raw.githubusercontent.com/benzinger300-debug/stattrakker-app/main"
BK="/root/nginx-backup-$TS"

echo "==> Backing up nginx config to $BK"
mkdir -p "$BK"
cp -a /etc/nginx/sites-available "$BK"/ 2>/dev/null
cp -a /etc/nginx/sites-enabled  "$BK"/ 2>/dev/null

echo "==> Fetching landing files into /var/www/stattrakker"
mkdir -p /var/www/stattrakker
cd /var/www/stattrakker || { echo "ABORT: cannot cd"; exit 1; }
ok=1
for f in index.html demo-720.mp4 demo-poster.jpg sitemap.xml robots.txt stattrakker-logo.svg; do
  if wget -q -O "$f" "$RAW/$f"; then echo "  got $f"; else echo "  FAILED $f"; ok=0; fi
done
if [ "$ok" != "1" ]; then echo "ABORT: could not fetch all landing files (no changes made)"; exit 1; fi
chown -R www-data:www-data /var/www/stattrakker

echo "==> Writing new nginx site"
cat > /etc/nginx/sites-available/stattrakker <<'NGINX'
server {
    server_name www.stattrakker.com stattrakker.com;
    root /var/www/stattrakker;
    index index.html;
    proxy_read_timeout 3600;
    proxy_send_timeout 3600;

    # The Shiny app stays at its natural path, /own-it/
    location /own-it/ {
        proxy_pass http://127.0.0.1:3838/own-it/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # Everything else is the static landing page
    location / {
        try_files $uri $uri/ =404;
    }

    listen [::]:443 ssl ipv6only=on;
    listen 443 ssl;
    ssl_certificate /etc/letsencrypt/live/stattrakker.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/stattrakker.com/privkey.pem;
    include /etc/letsencrypt/options-ssl-nginx.conf;
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem;
}
server {
    listen 80 default_server;
    listen [::]:80 default_server;
    server_name _;
    return 301 https://stattrakker.com$request_uri;
}
NGINX

# Private mode: if /etc/nginx/.htpasswd exists, password-protect the whole site
# (landing + app). Delete that file and re-run to go public again.
if [ -f /etc/nginx/.htpasswd ]; then
  sed -i '/server_name www.stattrakker.com stattrakker.com;/a\    auth_basic "Stattrakker — Private"; auth_basic_user_file /etc/nginx/.htpasswd;' /etc/nginx/sites-available/stattrakker
  echo "==> PRIVATE MODE ON (login required)"
else
  echo "==> Public mode (no login required)"
fi

echo "==> Enabling the site (preserving app.stattrakker.com)"
rm -f /etc/nginx/sites-enabled/default /etc/nginx/sites-enabled/stattrakker
ln -sf /etc/nginx/sites-available/stattrakker /etc/nginx/sites-enabled/stattrakker
# keep the app subdomain enabled if it's set up
[ -f /etc/nginx/sites-available/app-stattrakker ] && ln -sf /etc/nginx/sites-available/app-stattrakker /etc/nginx/sites-enabled/app-stattrakker

echo "==> Validating nginx config"
if nginx -t 2>/tmp/ngtest; then
    systemctl reload nginx
    echo "RESULT: PUBLISHED_OK  (landing at /, app at /own-it/)"
else
    echo "RESULT: NGINX_TEST_FAILED — reverting, no downtime"
    cat /tmp/ngtest
    rm -f /etc/nginx/sites-enabled/*
    cp -a "$BK"/sites-enabled/. /etc/nginx/sites-enabled/ 2>/dev/null
    nginx -t && systemctl reload nginx
    echo "RESULT: REVERTED  (your site is unchanged; backup at $BK)"
fi
