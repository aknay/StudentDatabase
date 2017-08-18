# --- !Ups

CREATE TABLE "student_table"("id" SERIAL PRIMARY KEY ,"name" varchar(200), "team_name" varchar(200),
"institution" varchar(200), "country" varchar(200), "league" varchar(200), "sub_league" varchar(200),
"event" varchar(200), "last_update_time" varchar(200), "update_by" INTEGER
);

# --- !Downs
DROP TABLE "userinfotable";
DROP TABLE "usertable" CASCADE;
DROP TABLE "student_table";
