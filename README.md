# x11-sentinel-server

Mouse cursor data collector and screen locker application backend server,
implemented in Erlang.

## Configurations

The application can be configured with the following environment variables:

### Backend server

*   `APP_DB_HOST`
    Hostname of the database used by the server.
    Default value: "db".

*   `APP_DB_USERNAME`
    Username that is needed for the database connection.
    Default value: "xss".

*   `APP_DB_PASSWORD`
    Password that is needed for the database connection.
    Default value: "secret".

*   `APP_DB_PORT`
    Exposed port of the database used by the server.
    Default value: 5432.

*   `APP_DB_NAME`
    Name of database that is needed for the database connection.
    Default value: "xss".

*   `APP_EVALUATION_SERVICE_HOST`
    Hostname of the evaluation service used by the server.
    Default value: "evaluation".

*   `APP_EVALUATION_SERVICE_PORT`
    Exposed port of the evaluation service used by the server.
    Default value: 4000.

*   `APP_MINIMUM_ELAPSED_TIME_FOR_FAILED_PROFILE_REBUILD`
    Minimum time in milliseconds after a failed profile can be rebuilt.
    Default value: 3600000 (1 hour).

*   `APP_MINIMUM_EVENT_COUNT_FOR_VERIFICATION`
    The user needs to have at least this many events in his / her latest stream
    for the server to start a verification.
    Default value: 7200.

*   `APP_MINIMUM_EVENT_COUNT_FOR_PROFILE`
    The user needs to have at least this many events for the server to start
    profile building.
    Default value: 1000000 (1_000_000).

*   `APP_NAME`
    The name of the application.
    Default value: "x11_sentinel_server".

*   `APP_PORT`
    Exposed port of the application.
    Default value: 8081.

*   `APP_SECRET_COOKIE`
    Erlang secret cookie.

*   `NODE_NAME`
    The name of the Erlang node.

*   `REBAR_PROFILE`
    Rebar profile used when building the server.
    Default value: "production".

*   `RUNTIME_ENV`
    The name of the runtime environment.


### Evaluation mock service

*   `EVALUATION_PORT`
    Exposed port of the mock evaluation service used by the server. This
    variable is used by the mock evaluation service which can be found under the
    `evaluation` directory.
    Default value: 4000.

*   `EVALUATION_PROFILE_BUILD_WAIT_TIME`
    This variable configures the time in milliseconds the mock evaluation service
    waits before returning the result of a profile build request.
    Default: 60000 (1 minute).

*   `EVALUATION_VERIFY_WAIT_TIME`
    This variable configures the time in milliseconds the mock evaluation service
    waits before returning the result of a verify request.
    Default: 5000.

*   `EVALUATION_RESPONSE_DIR`
    The mock evaluation service serves configurable JSON responses for profile
    building and verification requests. This variable configures the directory
    where these responses can be found.
    Default value: "/srv/data/".

### Postgres database

*   `POSTGRES_USER`
    Postgres database user.
    Default value: "postgres".

*   `POSTGRES_PASSWORD`
    Postgres database password.
    Default value: "postgres".

### Postgres database admin dashboard (only in test environment)

*   `PGADMIN_DEFAULT_EMAIL`
    Postgres admin dashboard email address used at login.
    Default value: "admin@admin.com"

*   `PGADMIN_DEFAULT_PASSWORD`
    Postgres admin dashboard password used at login.
    Default value: "root"

## Building and running the project

### Dependencies

*   **erlang**
    Install version at least `25.0`. Recommended way of installation for
    Linux based environments is via the `asdf` package manager.

*   **rebar**
    Install version at least `3.18.0`. Recommended way of installation for
    Linux based environments is via the `asdf` package manager.

### Building and running the project with Docker

To build and run the project in a local developer environment follow the
instructions below:

1.  Build the project with the following command:

    ```
    $ docker-compose --env-file env.local -f docker-compose.yml build
    ```

2.  Create docker network:

    ```
    $ docker network create x11_sentinel_server
    ```

3.  Start the containers:

    ```
    $ docker-compose --env-file env.local -f docker-compose.yml up -d
    ```

    This will create the volumes used by the containers.

4.  Stop the containers:

    ```
    $ docker-compose --env-file env.local -f docker-compose.yml down
    ```

5.  Switch to root user (If the Docker server is deployed on a different machine
    from the user's host machine, then login to the machine where the Docker
    server is deployed as root).

    Add read / write access to the services over their own volumes by changing
    the owner of the following directories:

    ```
    $ chown nobody:nogroup \
        /var/lib/docker/volumes/x11-sentinel-server_db-data/_data \
        /var/lib/docker/volumes/x11-sentinel-server_evaluation-response-data/_data
    ```

6.  Start the container of the database.

    ```
    $ docker-compose --env-file env.local -f docker-compose.yml up -d db
    ```

7.  Copy the migration scripts to the container of the database.

    ```
    $ docker cp priv/migration-scripts/. x11-sentinel-server_db_1:migration-scripts/
    ```

8.  Run the first migration script.

    ```
    $ docker exec -it x11-sentinel-server_db_1 \
        psql -h localhost -p 5432 -U postgres \
             -f migration-scripts/01_initialize_db_UP.sql
    ```

9.  Run the second migration script on the `xss` database.

    ```
    $ docker exec -it x11-sentinel-server_db_1 \
        psql -h localhost -p 5432 -U postgres \
        -d xss \
        -f migration-scripts/02_initialize_tables_UP.sql
    ```

10.  Stop the containers:

    ```
    $ docker-compose --env-file env.local -f docker-compose.yml down
    ```

11. Copy the mock evaluation service response JSON files to the volume.

    ```
    $ sudo cp -r \
        evaluation/response/* \
        /var/lib/docker/volumes/x11-sentinel-server_evaluation-response-data/_data/
    ```

12.  Start the containers:

    ```
    $ docker-compose --env-file env.local -f docker-compose.yml up -d
    ```

### Test environment

To build and run the project in a test environment use the `env.test` and the
`docker-compose.override.test.yml` configuration files when setting up the
environment. Deploying the project in a test environment is different from the
local environment in the following aspects:

*   The port of the database container is exposed to port 5432. This makes it
    possible to run tests including execution of database transactions.

*   There is a database admin page GUI (pgAdmin) which can be accesed on
    http://localhost:5050

*   The port of the mock evaluation server is exposed on port 4000. This is only
    for debugging purposes.

## Registering a new user

To register a new user to the database follow these intructions:

1.  Start a remote Erlang shell on the backend server's container.

    ```
    $ docker exec -it x11-sentinel-server_backend_1 bin/x11_sentinel_server remote
    ```

2.  Register the user by adding it to the database (if it is not already there):

    ```
    > UserId = <<"insert-user-id-here">>,
    > ok = xss_utils:maybe_add_user(UserId).
    ```

    Exit the shell by pressing Ctrl + c, then a.
