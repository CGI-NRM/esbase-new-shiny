# Esbase-new-shiny
The shiny app of the new Esbase system

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
app
└── mod_provberedning
    ├── mod_biologdata
    └── mod_provlista
</pre>
