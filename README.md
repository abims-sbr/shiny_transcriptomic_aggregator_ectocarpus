# Shiny Transcriptomic Aggregator

## Requirements

- R (≥3.0.2)

```
sudo yum install R
```

- Shiny

```
sudo su - \
	-c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
```

- NGINX <!--(≥1.12.2)-->


## Installation

### Shiny server installation

Following [this tutorial](https://www.linode.com/docs/development/r/how-to-deploy-rshiny-server-on-ubuntu-and-debian/).

1. Install `gdebi` :

	`sudo apt-get install gdebi-core`

2. Download Shiny Server

	`wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.6.875-amd64.deb`

3. Use `gdebi` to install the Shiny Server package :

	`sudo gdebi shiny-server-1.5.6.875-amd64.deb`

4. The `shiny-server` service should start automatically. Check its status :

	`sudo systemctl status shiny-server.service`

5. In a browser, navigate to your public IP address or FQDN on port `3838` (e.g. `example.com:3838`). You should see the Shiny Server welcome page:

![](https://www.linode.com/docs/development/r/how-to-deploy-rshiny-server-on-ubuntu-and-debian/shiny-welcome.png)

6. Give the right to the shiny user in `/srv/shiny-server`

        `chown -R shiny:shiny /srv/shiny-server`

### Application installation

- Clone the application repository from https://github.com/abims-sbr/shiny_transcriptomic_aggregator_ectocarpus/ in `/srv/shiny-server/` :

	`cd /srv/shiny-server/`
	
	`git clone https://github.com/abims-sbr/shiny_transcriptomic_aggregator_ectocarpus.git`

- Install R packages as root :

	`cd shiny_transcriptomic_aggregator_ectocarpus`

	`Rscript packages.R`


## Configuration

### Shiny server configuration

/etc/shiny-server/shiny-server.conf

```
# Instruct Shiny Server to run applications as the user "shiny"
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {

    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
}
```

### Nginx configuration

Add few lines in `/etc/nginx/nginx.conf` for websockets

```
http {
    map $http_upgrade $connection_upgrade {
        default upgrade;
        ''      close;
    }
}
```

Create `/etc/nginx/conf.d/sta.conf` file

```
server {
        listen 80       default_server;
        listen [::]:80  default_server;
        server_name _;
    
        location /ectocarpus/ {
                proxy_pass      http://127.0.0.1:3838/shiny_transcriptomic_aggregator_ectocarpus/public/;
                proxy_redirect  off;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection $connection_upgrade;
        }

        location /ectocarpus/private/ {
                proxy_pass      http://127.0.0.1:3838/shiny_transcriptomic_aggregator_ectocarpus/private/;
                proxy_redirect  off;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection $connection_upgrade;
                
                auth_ldap "Connection to your sb-roscoff account required";
                auth_ldap_servers sb-roscoff;
        }
}
```

Add a `/etc/shiny-server/conf.d/ldap-auth.conf` file and complete it

```
ldap_server ldap_name {
        url ldap_url;
        group_attribute uniquemember;
        group_attribute_is_dn on;
        # list of allowed users
        require user "uid=,ou=,dc=,dc=";
}

```


### Application configuration

2 instances provided : One public and one private.

The configuration of your app can be change in the private/conf.R and public/conf.R files.

```R
### Shiny app configurations ###

# Name of the project
project <- "Project Name"

# Instance status ("public" OR "private")
instance_tag <- "public"

# Columns to ignore in metadata file
gene_col_blacklist <- c("description", "content")
sample_col_blacklist <- c("replicats")

# Project Files
tpms_input <- "data/TPMs.csv"
genes_data_input <- "data/Genes_data.csv"
samples_data_input <- "data/Samples_data.csv"

```

### Note

Add the following block in the proxy conf for websockets

```
<VirtualHost *:443>
        ProxyPreserveHost On
        ProxyPass / http://rshiny.sb-roscoff.fr/
        ProxyPassReverse / http://rshiny.sb-roscoff.fr/
        RewriteEngine on
        RewriteCond %{HTTP:Upgrade} =websocket
        RewriteRule /(.*) ws://rshiny.sb-roscoff.fr/$1 [P,L]
        RewriteCond %{HTTP:Upgrade} !=websocket
        RewriteRule /(.*) http://rshiny.sb-roscoff.fr/$1 [P,L]
```

## Logs

Logs are available in `/var/log/shiny-server`


## Update Shiny Transcriptomic Aggregator 

### Update the code

```
cd /srv/shiny-server/shiny_transcriptomic_aggregator_ectocarpus
git pull origin master
```

### Update data

- Private :

```
scp path/to/your/data root@IP:/srv/shiny-server/shiny_transcriptomic_aggregator_ectocarpus/private/real-data/
```

- Public :

```
scp path/to/your/data root@IP:/srv/shiny-server/shiny_transcriptomic_aggregator_ectocarpus/private/real-data/
```


Verify `tpms_input`, `genes_data_input` and `samples_data_input` variables in `private/conf.R` and `public/conf.R` files have the good file path.