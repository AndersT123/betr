create table mydb.results_bold (
  id int not null auto_increment,
  game_time datetime,
  home_team varchar(32),
  away_team varchar(32),
  home_score int,
  away_score int,
  primary key (id))