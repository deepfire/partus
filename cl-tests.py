from cl import *

@block
def outer_m(yoo):
	@block
	def midder():
		(lambda: return_from("midder", "lambda way!"))()
		return "Middah!"
	return midder()

@block
def outer_l(yoo):
	@block
	def midder():
		(lambda: return_from("outer_l", "lambda way!"))()
		return "Middah!"
	return midder()

assert(outer_m("Yeehaw!") == "lambda way!")
assert(outer_l("Yeehaw!") == "lambda way!")

