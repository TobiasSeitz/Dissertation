# SQL

## this query gives us the average rounds played on the first game. 
SELECT firstStep.uA, AVG(rounds) FROM 
	(SELECT `user` as uA, gameIndex, COUNT(userID) AS `rounds` 
		FROM `pasdjo-rounds` 
		WHERE gameIndex <> 1 
		GROUP BY `user`, gameIndex) AS firstStep 
	GROUP BY firstStep.uA;

## this query gives us the average rounds played per game after the first game.
SELECT firstStep.uA, AVG(rounds) FROM 
	(SELECT `user` as uA, gameIndex, COUNT(userID) AS `rounds` 
		FROM `pasdjo-rounds` 
		WHERE gameIndex <> 1 
		GROUP BY `user`, gameIndex) AS firstStep 
	GROUP BY firstStep.uA;