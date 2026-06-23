#!/bin/bash
# Stattrakker one-shot server deploy. Run on the droplet as root:
#   curl -fsSL https://raw.githubusercontent.com/benzinger300-debug/stattrakker-app/main/deploy.sh | bash
set -u
export DEBIAN_FRONTEND=noninteractive
APP_URL="https://raw.githubusercontent.com/benzinger300-debug/stattrakker-app/main/app.R"
DOMAIN="stattrakker.com"
EMAIL="benzinger300@gmail.com"

echo "==> [1/6] System packages"
apt-get update -qq
apt-get install -y -qq wget curl gpg ca-certificates gdebi-core nginx certbot python3-certbot-nginx

echo "==> [2/6] R"
if ! command -v R >/dev/null 2>&1; then
  wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | gpg --dearmor -o /usr/share/keyrings/r-project.gpg
  echo "deb [signed-by=/usr/share/keyrings/r-project.gpg] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" > /etc/apt/sources.list.d/r-cran.list
  apt-get update -qq
  apt-get install -y -qq r-base r-base-dev libssl-dev libcurl4-openssl-dev libxml2-dev
fi

echo "==> [3/6] Shiny Server"
if [ ! -f /usr/bin/shiny-server ] && [ ! -x /opt/shiny-server/bin/shiny-server ]; then
  wget -q https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb -O /tmp/ss.deb
  gdebi -n /tmp/ss.deb
fi
Rscript -e "if(!requireNamespace('shiny',quietly=TRUE)) install.packages('shiny', repos='https://cran.rstudio.com/')"

echo "==> [4/6] Deploy app.R"
mkdir -p /srv/shiny-server/own-it/data
wget -O /srv/shiny-server/own-it/app.R "$APP_URL"
chown -R shiny:shiny /srv/shiny-server/own-it
systemctl enable shiny-server >/dev/null 2>&1
systemctl restart shiny-server

echo "==> [5/6] nginx"
cat > /etc/nginx/sites-available/${DOMAIN} <<'NGINX'
server {
    listen 80;
    server_name stattrakker.com www.stattrakker.com;
    proxy_read_timeout 3600;
    proxy_send_timeout 3600;
    location / {
        proxy_pass http://127.0.0.1:3838/own-it/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_redirect http://127.0.0.1:3838/own-it/ /;
    }
}
NGINX
ln -sf /etc/nginx/sites-available/${DOMAIN} /etc/nginx/sites-enabled/${DOMAIN}
rm -f /etc/nginx/sites-enabled/default
nginx -t && systemctl reload nginx

echo "==> [6/6] HTTPS certificate"
certbot --nginx -d ${DOMAIN} -d www.${DOMAIN} --non-interactive --agree-tos --email ${EMAIL} --redirect || \
  echo "   (SSL step had an issue — site still works on http; rerun certbot later if needed)"

echo ""
echo "=================================================="
echo "  DONE.  Visit:  https://${DOMAIN}"
echo "=================================================="
