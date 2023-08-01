# Esbase-new-shiny
The shiny app of the new Esbase system

# Docker-compose secret and .env
To run the application, copy the `docker-compose.yml` file, copy `.env.example` into `.env` and `db_password.example.txt` into `db_password.txt`. Enter your details into these files.

Then run `docker-compose pull` and `docker-compose up -d`.

# Connect to MariaDB on host
[Create user](https://mariadb.com/kb/en/create-user/) on the host database with username 'docker' and password 'dockerpass'.

From docker connect to 172.17.0.1 (ip of docker0 on host)

On host, allow connections from (172.17.0.1), docker0:

[By adding the following lines to `/etc/my.cnf`](https://mariadb.com/kb/en/configuring-mariadb-for-remote-client-access/):
```
[mysqld]
	bind-address = 172.17.0.1
```

This ip might not be needed:
```sh
docker exec -it esbase-shiny sh -c 'sudo apt update && sudo apt install iproute2 && ip addr show eth0'
```

# Shiny Modules

<pre>
</pre>

# Journal: Create user and connect to DB on nogrod-clone 

Ip of my computer on nrm network: 172.16.28.143, confirmed by pinging from nogrod-clone.nrm.se and running `tcpdump ip proto \\icmp` on local computer.

Binding mysql server to 0.0.0.0

Adding 
`sudo iptables -I RH-Firewall-1-INPUT 5 -p tcp -s 172.16.28.143 --dport 3306 -j ACCEPT`

Connection worked to user with host `%`, and all privileges (eliaslund in my case).

Trying to connect to nogrod-clone.nrm.se from nrm-shiny-esbase.nrm.se, getting `Bad handshake`.
Trying to connect to nogrod-clone.nrm.se from esbase-new-shiny docker container on laptop, getting `Bad handshake`.

I can now connect to the mysql server from within the docker-containers using the mariadb-client-cli, still getting error when trying to connect using RMariaDB in R from within the docker-containers.

Python worked using MariaDB/Connector C, testing if building RMariaDB from source could resolve the issue.

It worked! :)
