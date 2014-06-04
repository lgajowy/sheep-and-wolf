module ParserTest where 

import InputParser



test = do 
	parse coord "1, 2"

test2 = do
	parse coord "3,    2"

test3 = do
	parse coord "12,    2"

test4 = do
	parse coord "1,  2)"

test5 = do 
	coordinates <- return getLine
	parse coord coordinates


