BEGIN;

CREATE TABLE game 
    ( elve_move CHAR(1)
    , my_move CHAR(1)
    );

COPY game(elve_move, my_move)
FROM '/home/sebsheep/progs_div/advent-of-code/2022/02/input.csv'
DELIMITER ' '
;
-- PART 1
CREATE VIEW scores AS
SELECT 
    elve_move,
    my_move,
    CASE my_move
        WHEN 'X' THEN 1
        WHEN 'Y' THEN 2
        WHEN 'Z' THEN 3
    END 
    AS symbol
    ,
    CASE my_move
        WHEN 'X' THEN
            CASE elve_move
                WHEN 'A' THEN 3
                WHEN 'B' THEN 0
                WHEN 'C' THEN 6
            END
        WHEN 'Y' THEN
            CASE elve_move
                WHEN 'A' THEN 6
                WHEN 'B' THEN 3
                WHEN 'C' THEN 0
            END
        WHEN 'Z' THEN
            CASE elve_move
                WHEN 'A' THEN 0
                WHEN 'B' THEN 6
                WHEN 'C' THEN 3
        END
    END 
    AS round
FROM game;

SELECT SUM(symbol + round) AS part_1
FROM scores;

-- PART 2

CREATE VIEW game_parsed AS
SELECT 
    elve_move,
    CASE my_move
        WHEN 'X' THEN
            CASE elve_move
                WHEN 'A' THEN 'Z'
                WHEN 'B' THEN 'X'
                WHEN 'C' THEN 'Y'
            END
        WHEN 'Y' THEN
            CASE elve_move
                WHEN 'A' THEN 'X'
                WHEN 'B' THEN 'Y'
                WHEN 'C' THEN 'Z'
            END
        WHEN 'Z' THEN
            CASE elve_move
                WHEN 'A' THEN 'Y'
                WHEN 'B' THEN 'Z'
                WHEN 'C' THEN 'X'
        END
    END 
    AS my_move
FROM game;

CREATE VIEW scores_2 AS
SELECT 
    elve_move,
    my_move,
    CASE my_move
        WHEN 'X' THEN 1
        WHEN 'Y' THEN 2
        WHEN 'Z' THEN 3
    END 
    AS symbol
    ,
    CASE my_move
        WHEN 'X' THEN
            CASE elve_move
                WHEN 'A' THEN 3
                WHEN 'B' THEN 0
                WHEN 'C' THEN 6
            END
        WHEN 'Y' THEN
            CASE elve_move
                WHEN 'A' THEN 6
                WHEN 'B' THEN 3
                WHEN 'C' THEN 0
            END
        WHEN 'Z' THEN
            CASE elve_move
                WHEN 'A' THEN 0
                WHEN 'B' THEN 6
                WHEN 'C' THEN 3
        END
    END 
    AS round
FROM game_parsed;


SELECT SUM(symbol + round) AS part_2
FROM scores_2;

