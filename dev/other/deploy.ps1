cd C:/Users/maxim/Unsync/curbcut-montreal
heroku login
heroku container:login
heroku container:push web -a cc-montreal
heroku container:release web -a cc-montreal
