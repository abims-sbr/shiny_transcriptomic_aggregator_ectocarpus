# Shiny Transcriptomic Aggregator

## Requirements

- R (≥3.0.2)

- Shiny

	`sudo su - \
	-c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""`

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


### Application installation

- Clone the application repository from https://github.com/abims-sbr/shiny_transcriptomic_aggregator_ectocarpus/ in `/srv/shiny-server/` :

	`cd /srv/shiny-server/`
	
	`git clone https://github.com/abims-sbr/shiny_transcriptomic_aggregator_ectocarpus.git`



## Configuration

### Nginx configuration

```
http {

	ldap_server YOUR_LDAP_SERVER {
        ...
    }

	server {
	        listen 80       default_server;
	        listen [::]:80  default_server;
	        server_name _;
	    
	        location / {
	                proxy_pass      http://127.0.0.1:3838/;
	                proxy_redirect  http://127.0.0.1:3838/ $scheme://$host/;
	        }

	        location /shiny_transcriptomic_aggregator/ {
	                proxy_pass      http://127.0.0.1:3838/shiny_transcriptomic_aggregator/;
	                proxy_redirect  off;
	        }

	        location /shiny_transcriptomic_aggregator/private/ {
	                proxy_pass      http://127.0.0.1:3838/shiny_transcriptomic_aggregator/private/;
	                proxy_redirect  off;
	                
	                auth_ldap "Connection to your account required";
	                auth_ldap_servers YOUR_LDAP_SERVER;
	    	}

	        location /shiny_transcriptomic_aggregator/public/ {
	                proxy_pass      http://127.0.0.1:3838/shiny_transcriptomic_aggregator/public/;
	                proxy_redirect  off;
	        }
	}
}
```

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

### Application configuration

2 instances provided : One public and one private.

The configuration of your app can be change in the private/app-conf.R and public/app-conf.R files.

```R
### Shiny app configurations ###

# Name of the project
project <- "Project Name"

# Instance status ("public" OR "private")
instance_tag <- "public"

# Project Files
tpms_input <- "real-data/TPMs.csv"
genes_data_input <- "real-data/Genes_data.csv"
samples_data_input <- "real-data/Samples_data.csv"

# Columns to ignore in metadata file
gene_col_blacklist <- c("description", "content")
sample_col_blacklist <- c("replicats", "sample_name")
```
