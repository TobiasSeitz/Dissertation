# SQL

## this query gives us the number of rounds played on the first game. 
SELECT firstStep.uA, AVG(rounds) FROM 
	(SELECT `user` as uA, gameIndex, COUNT(userID) AS `rounds` 
		FROM `pasdjo-rounds-1y` 
		WHERE gameIndex = 1 
		GROUP BY `user`, gameIndex) AS firstStep 
	GROUP BY firstStep.uA;

## this query gives us the average rounds played per game after the first game.
SELECT firstStep.uA, AVG(rounds) FROM 
	(SELECT `user` as uA, gameIndex, COUNT(userID) AS `rounds` 
		FROM `pasdjo-rounds-1y` 
		WHERE gameIndex <> 1 
		GROUP BY `user`, gameIndex) AS firstStep 
	GROUP BY firstStep.uA;





### for new analyses
# create view:
select 
`research`.`pasdjo-rounds-1y`.`user` AS `user`,
`research`.`pasdjo-rounds-1y`.`condition` AS `condition`,
`research`.`pasdjo-rounds-1y`.`gameIndex` AS `gameIndex`,
avg(`research`.`pasdjo-rounds-1y`.`tendency`) AS `tendency`,
avg(`research`.`pasdjo-rounds-1y`.`guessesLog10`) AS `guessesLog10`,
avg(`research`.`pasdjo-rounds-1y`.`guesses`) AS `guesses`,
avg(`research`.`pasdjo-rounds-1y`.`score`) AS `score`,
avg(`research`.`pasdjo-rounds-1y`.`userRating`) AS `userRating`,
max(DATE_FORMAT(FROM_UNIXTIME(`research`.`pasdjo-rounds-1y`.`timestamp` / 1000), '%e %b %Y')) AS `timestamp`
	from `research`.`pasdjo-rounds-1y` 
	WHERE `timestamp` <= 1510876800000
	group by `research`.`pasdjo-rounds-1y`.`condition`,
	`research`.`pasdjo-rounds-1y`.`user`,
	`research`.`pasdjo-rounds-1y`.`gameIndex` 
	order by `research`.`pasdjo-rounds-1y`.`user`,
	`research`.`pasdjo-rounds-1y`.`gameIndex`,
	`research`.`pasdjo-rounds-1y`.`condition`;


	max(`research`.`pasdjo-rounds-1y`.`timestamp`) AS `timestamp` 