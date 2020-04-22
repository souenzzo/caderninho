#!/usr/bin/env make


dev-db:
	docker run --name my-postgres --env=POSTGRES_PASSWORD=postgres --rm -p 5432:5432 postgres:alpine



dev-db-init:
	cat resources/create-dev-db.sql | docker exec -i my-postgres psql -U postgres
