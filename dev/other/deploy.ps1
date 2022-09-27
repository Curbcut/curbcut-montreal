cd C:/Users/maxim/Unsync/Sus
heroku login
heroku container:login
heroku container:push web -a sus-mcgill-test
heroku container:release web -a sus-mcgill-test
