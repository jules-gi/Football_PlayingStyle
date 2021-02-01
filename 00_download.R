# Date   : 2020-03-03
# Author : Girard Jules (jules.girard@outlook.com)

# Load packages
packages_list = c('here', 'stringr', 'StatsBombR', 'ggplot2')
lapply(packages_list, library, character.only=TRUE)


# Create functions
{
  # Opposite of %in%
  `%!in%` = Negate(`%in%`)
  
  replace_coords = function(X, column_name, get_Z=FALSE) {
    
    ### Convert column location from c(X, Y, Z)
    ### to 2 (or 3) independants columns X, Y (and Z).
    ###
    ### Inputs :
    ###   - X (data.frame), events data
    ###   - column_name (string), column to split
    ###   - get_Z (bollean, default=FALSE), if Z location or not
    ### Output :
    ###   - X (data.frame), events data after transformation
    
    get_x_coord = function(location) {
      x = location[1]
      return(x)
    }
    get_y_coord = function(location) {
      y = location[2]
      return(y)
    }
    get_z_coord = function(location) {
      z = location[3]
      return(z)
    }
    
    values = X[,which(colnames(X) == column_name)]
    values[sapply(values, is.null)] = NA
    
    name_X = paste0(column_name, '_X')
    name_Y = paste0(column_name, '_Y')
    X[[name_X]] = as.numeric(unlist(sapply(values, get_x_coord)))
    X[[name_Y]] = as.numeric(unlist(sapply(values, get_y_coord)))
    if (get_Z) {
      name_Z = paste0(column_name, '_Z')
      X[[name_Z]] = as.numeric(unlist(sapply(values, get_z_coord)))
    }
    
    X = X[,-which(colnames(X) == column_name)]
    
    return(X)
  }
  
  get_lineup = function(lineup, team, formation, time) {
    
    ### Extract lineup and add contextual informations
    ###
    ### Inputs :
    ###     - lineup (data.frame), original lineup from StatsBomb
    ###     - team (character), team name 
    ###     - formation (integer), formation of the lineup
    ###     - time (numeric), time of the game
    ### Output :
    ###     - lineup (data.frame), lineup transformed
    
    lineup$team.name = team
    lineup$formation = formation
    lineup$time.start = time
    
    order = match(c('team.name', 'formation', 'player.name', 'position.name', 'time.start'), colnames(lineup))
    return(lineup[,order])
  }
}


# Download data
competitions = FreeCompetitions()
matches = FreeMatches(competitions[which(competitions$competition_name == 'FIFA World Cup'),])

# Variables to store
columns_var = c('period', 'time', 'type.name', 'location_X', 'location_Y', 'player.name', 'team.name', 'possession', 'possession_team.name',
                'play_pattern.name', 'under_pressure', 'counterpress', 'pass.end_location_X', 'pass.end_location_Y', 'pass.recipient.name',
                'pass.length', 'pass.angle', 'pass.switch', 'pass.cross', 'pass.shot_assist', 'pass.goal_assist', 'pass.cut_back', 'pass.aerial_won',
                'pass.deflected', 'pass.height.name', 'pass.body_part.name', 'pass.type.name', 'pass.outcome.name', 'carry.end_location_X',
                'carry.end_location_Y', 'dribble.outcome.name', 'duel.type.name', 'duel.outcome.name', 'shot.end_location_X', 'shot.end_location_Y',
                'shot.end_location_Z', 'shot.outcome.name', 'shot.statsbomb_xg', 'shot.first_time', 'shot.aerial_won', 'shot.open_goal', 'shot.body_part.name',
                'foul_committed.offensive', 'foul_won.defensive', 'interception.outcome.name', 'clearance.aerial_won')

# Create databases
id_database = NULL
lineup_database = NULL
event_database = NULL
for (m in 1:nrow(matches)) {
  print(paste('Match', m, '/', nrow(matches)))
  match_infos = list()
  match_infos$date = as.character(matches[m, which(colnames(matches) == 'match_date')])
  match_infos$eq_dom = as.character(matches[m, which(colnames(matches) == 'home_team.home_team_name')])
  match_infos$eq_ext = as.character(matches[m, which(colnames(matches) == 'away_team.away_team_name')])
  comp_stage = as.character(matches[m, which(colnames(matches) == 'competition_stage.name')])
  match_infos$comp_stage = ifelse(comp_stage == 'Group Stage', as.character(matches[m, which(colnames(matches) == 'home_team.home_team_group')]), comp_stage)
  
  # Each match.id refere to match informations
  id_database = rbind(id_database,
                      as.matrix(cbind(match.id = m-1,
                                      date = match_infos$date,
                                      comp_stage = match_infos$comp_stage,
                                      team.home = match_infos$eq_dom,
                                      team.away = match_infos$eq_ext))
  )
  
  # Download match
  match = get.matchFree(matches[m,])
  
  # Get time in minute only
  match$time = round(match$minute + (match$second/60), 2)
  
  # Get additional times
  match_end = ifelse(length(which(match$period == 4)) > 0, 120, 90)
  time_limits = c(45, 90, 105, 120)
  additional_time = c(
    first.half = max(match$time[which(match$period == 1)]) - time_limits[1],
    second.half = max(match$time[which(match$period == 2)]) - time_limits[2],
    ot.first.half = ifelse(length(which(match$period == 3)) > 0, max(match$time[match$period == 3]) - time_limits[3], 0),
    ot.second.half = ifelse(length(which(match$period == 4)) > 0, max(match$time[match$period == 4]) - time_limits[4], 0)
  )
  
  # Feed lineup database
  for (team in c(match_infos$eq_dom, match_infos$eq_ext)) {
    substitution_add_time = list()
    lineup_add_time = list()
    
    # Get starting team
    starting_team = match$tactics.lineup[which(match$type.name == 'Starting XI' & match$team.name == team)][[1]]
    formation = match$tactics.formation[which(match$type.name == 'Starting XI' & match$team.name == team)]
    lineup_team = cbind(match.id=m-1,
                        get_lineup(starting_team, team, formation, time=0),
                        time.end=match_end
    )
    
    # Get substitutions and tactical adjustements
    lineup_index = which(match$type.name %in% c('Tactical Shift', 'Substitution') & match$team.name == team)
    for (i in lineup_index) {
      time = match$time[i]
      period = match$period[i]
      if (match$type.name[i] == 'Substitution') {
        # If it's a substitutions
        player_out = match$player.name[i]
        player_in = match$substitution.replacement.name[i]
        position = lineup_team$position.name[tail(which(lineup_team$player.name == player_out), 1)]
        lineup_team$time.end[which(lineup_team$player.name == player_out)] = time
        lineup_team = rbind(lineup_team,
                            c(match.id=m-1,
                              team.name=team,
                              formation=formation,
                              player.name=player_in,
                              position.name=position,
                              time.start=time,
                              time.end=match_end
                            )
        )
        
        # Store if the player go in the field during the additional_time
        if (time_limits[period] < time) {
          substitution_add_time[[length(substitution_add_time)+1]] = c(index_player=nrow(lineup_team),
                                                                       time.start=time_limits[period],
                                                                       additional_time[period]-(time-time_limits[period])
          )
        }
      } else {
        # If it's a tactical shift
        formation = match$tactics.formation[i]
        new_lineup = match$tactics.lineup[i][[1]]
        lineup_team$time.end[which(lineup_team$time.end == match_end)] = time
        lineup_team = rbind(lineup_team,
                            cbind(match.id=m-1,
                                  get_lineup(new_lineup, team, formation, time),
                                  time.end=match_end
                            )
        )
        
        # Store if the lineup change in the additional time
        if (time_limits[period] < time) {
          lineup_add_time[[length(lineup_add_time)+1]] = c(index_player=list(which(lineup_team$time.end == match_end)),
                                                           time.start=time_limits[period],
                                                           additional_time[period]-(time-time_limits[period])
          )
        }
      }
    }
    lineup_team$time.start = as.numeric(lineup_team$time.start)
    lineup_team$time.end = as.numeric(lineup_team$time.end)
    
    # Add additional time to players who were on the pitch
    lineup_team$first.half = 0
    lineup_team$second.half = 0
    lineup_team$ot.first.half = 0
    lineup_team$ot.second.half = 0
    for (i in 1:4) {
      index_add_time = which(lineup_team$time.start < time_limits[i] & lineup_team$time.end >= time_limits[i])
      lineup_team[index_add_time, which(colnames(lineup_team) == names(additional_time)[i])] = additional_time[i]
    }
    
    # Change additional time to players and lineup who changed on the field during additional time
    if (length(substitution_add_time) > 0) {
      for (i in 1:length(substitution_add_time)) {
        lineup_team$time.start[substitution_add_time[[i]]['index_player']] = substitution_add_time[[i]]['time.start']
        lineup_team[substitution_add_time[[i]]['index_player'], which(colnames(lineup_team) == names(substitution_add_time[[i]])[3])] = substitution_add_time[[i]][3]
      }
    }
    if (length(lineup_add_time) > 0) {
      for (i in 1:length(lineup_add_time)) {
        lineup_team$time.start[lineup_add_time[[i]]$index_player] = lineup_add_time[[i]]$time.start
        lineup_team[lineup_add_time[[i]]$index_player, which(colnames(lineup_team) == names(lineup_add_time[[i]])[3])] = lineup_add_time[[i]][3]
      }
    }
    
    lineup_database = rbind(lineup_database, lineup_team)
  }
  
  # Get independant X, Y, Z coordinates 
  for (name in colnames(match)[which(str_detect(colnames(match), 'location'))]) {
    match = replace_coords(match, name, get_Z = ifelse(name == 'shot.end_location', TRUE, FALSE))
  }
  
  # Select variables
  columns_miss = which(columns_var %!in% colnames(match))
  if (length(columns_miss) > 0) {
    for (i in 1:length(columns_miss)) {
      match = cbind(match, NA)
    }
    colnames(match)[(ncol(match)-length(columns_miss)+1):ncol(match)] = columns_var[columns_miss]
  }
  match = match[, match(columns_var, colnames(match))]
  
  # Filter wrong data
  filter_rows = which(c(0, diff(match$time)) < 0 & str_detect(match$type.name, 'Half') == FALSE)
  if (length(filter_rows) > 0) {
    match = match[-filter_rows,]
  }
  match = match[-which(match$type.name %in% c('Starting XI', 'Substitution', 'Tactical Shift') | str_detect(match$type.name, 'Half')),]
  
  # Center position to the center of the field
  match[,which(str_detect(colnames(match), 'location_X'))] = match[,which(str_detect(colnames(match), 'location_X'))] - 60
  match[,which(str_detect(colnames(match), 'location_Y'))] = match[,which(str_detect(colnames(match), 'location_Y'))] - 40
  
  # Remove '*' character to type.name
  match$type.name[which(str_detect(match$type.name, '\\*'))] = str_sub(match$type.name[which(str_detect(match$type.name, '\\*'))], 1, -2)
  
  # Feed database
  event_database = rbind(event_database,
                         as.matrix(cbind(match.id = m-1, match))
  )
}
lineup_database = as.data.frame(lineup_database %>%
                                  mutate(time.play = (time.end-time.start) + (first.half+second.half+ot.first.half+ot.second.half))
)

# Save database
write.csv(event_database, paste0(here(), '/database/Events.csv'), row.names = FALSE)
write.csv(lineup_database, paste0(here(), '/database/Lineups.csv'), row.names = FALSE)
write.csv(lineup_database, paste0(here(), '/database/MatchesMetadata.csv'), row.names = FALSE)

db_events = read.csv(paste0(here(), "/database/Events.csv"))
for (i in 1:ncol(db_events)) {
  if (class(db_events[,i]) == "logical") {
    db_events[,i] = ifelse(is.na(db_events[,i]), FALSE, TRUE)
  } else if (class(db_events[,i]) == "numeric") {
    db_events[which(is.na(db_events[,i])),i] = NaN
  }
}

write.csv(db_events, paste0(here(), '/database/Events_PyUsage.csv'), row.names = FALSE)


