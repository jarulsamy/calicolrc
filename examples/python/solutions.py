#
# Pyjama - Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id: $

# This is the solution for Laura's MIT Python Homework
# D.S. Blank

import operator

def check(number, parts, factors):
	"""
	Checks to see if a number can be factored by parts * factors.
	"""
	if len(parts) != len(factors):
		return False
	return number == sum(map(operator.mul, parts, factors))

def factor(number, parts):
	if len(parts) == 0:
		return []
	part = parts[0]
	solutions = []
	for i in range(number / part, -1, -1):
		if number - i * part == 0:
			solutions.append([i])
		else:
			sub_factors = factor(number - i * part, parts[1:])
			for solution in sub_factors:
				if check(number, parts, [i] + solution):
					solutions.append([i] + solution)
	return solutions
