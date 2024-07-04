package main

import (
	"encoding/binary"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func textToPhonemes(input []byte) bool {
	var mem56PhonemeOutpos byte // output position for phonemes
	var mem57CurrentFlags byte
	inputTempIndex := byte(0)
	var mem59 byte
	var mem60InputMatchPos byte
	var mem61InputPos byte
	var mem62 uint16              // memory position of current rule
	var mem64EqualSignInRule byte // position of '=' or current character
	var mem65ClosingBrace byte    // position of ')'
	var mem66OpenBrace byte       // position of '('
	var y byte
	var r int
	processRuleFlag := false

	inputTemp[0] = ' '

	// secure copy of input because input will be overwritten by phonemes
	x = byte(0)
	for x < 255 {
		a := input[x] & 127
		if a >= 112 {
			a &= 95
		} else if a >= 96 {
			a &= 79
		}
		x++
		inputTemp[x] = a
	}
	inputTemp[255] = 27
	mem56PhonemeOutpos, mem61InputPos = 255, 255

	for {
		for {
			for {
				mem61InputPos++
				x = mem61InputPos
				mem64EqualSignInRule = inputTemp[x]
				if mem64EqualSignInRule == '[' {
					mem56PhonemeOutpos++
					x = mem56PhonemeOutpos
					input[x] = 155
					return true
				}

				if mem64EqualSignInRule != '.' {
					break
				}
				x++
				if (tab36376[inputTemp[x]] & 1) != 0 {
					break
				}
				mem56PhonemeOutpos++
				x = mem56PhonemeOutpos
				input[x] = '.'
			}
			mem57CurrentFlags = tab36376[mem64EqualSignInRule]
			if (mem57CurrentFlags & 2) != 0 {
				mem62 = 37541
				break
			}

			if mem57CurrentFlags != 0 {
				break
			}
			inputTemp[x] = ' '
			mem56PhonemeOutpos++
			x = mem56PhonemeOutpos
			if x > 120 {
				input[x] = 155
				return true
			}
			input[x] = 32
		}

		if (mem57CurrentFlags & 2) == 0 {
			if (mem57CurrentFlags & 128) == 0 {
				return false
			}

			// go to the right rules for this character.
			x = mem64EqualSignInRule - 'A'
			mem62 = uint16(tab37489[x]) | (uint16(tab37515[x]) << 8)
		}

		for {
			// find next rule
			mem62++ // Start checking from the next position
			for (getRuleByte(mem62, 0) & 128) == 0 {
				mem62++ // Continue searching in the next byte
			}

			// Initialize y to search for the opening brace '('
			y = 1 // Start checking from the next position
			for getRuleByte(mem62, y) != '(' {
				y++ // Increment y until '(' is found
			}
			mem66OpenBrace = y // Store the position of '('

			// Look for the closing brace ')'
			y++ // Start checking from the next position
			for getRuleByte(mem62, y) != ')' {
				y++ // Increment y until ')' is found
			}
			mem65ClosingBrace = y // Store the position of ')'

			// Look for the '=' sign, taking into account 127 mask
			y++ // Start checking from the next position
			for (getRuleByte(mem62, y) & 127) != '=' {
				y++ // Increment y until '=' is found
			}
			mem64EqualSignInRule = y // Store the position of '='
			mem60InputMatchPos = mem61InputPos
			x = mem60InputMatchPos
			// compare the string within the bracket
			y = mem66OpenBrace + 1

			matchFailed := false
			for {
				if getRuleByte(mem62, y) != inputTemp[x] {
					matchFailed = true
					break
				}
				y++
				if y == mem65ClosingBrace {
					break
				}
				x++
				mem60InputMatchPos = x
			}

			if matchFailed {
				continue
			}

			// the string in the bracket is correct
			mem59 = mem61InputPos

			for {
				for {
					mem66OpenBrace--
					mem57CurrentFlags = getRuleByte(mem62, mem66OpenBrace)
					if (mem57CurrentFlags & 128) != 0 {
						inputTempIndex = mem60InputMatchPos

						processRuleFlag = true
						break
					}
					x = mem57CurrentFlags & 127
					if (tab36376[x] & 128) == 0 {
						break
					}
					if inputTemp[mem59-1] != mem57CurrentFlags {
						matchFailed = true
						break
					}
					mem59--
				}
				if !processRuleFlag {
					if matchFailed {
						break
					}

					ch := mem57CurrentFlags

					r = handleCh2(ch, int(mem59-1), inputTemp)
					if r == -1 {
						switch ch {
						case '&':
							if !code37055(mem59-1, 16) {
								if inputTemp[x] != 'H' {
									r = 1
								} else {
									x--
									a := inputTemp[x]
									if (a != 'C') && (a != 'S') {
										r = 1
									}
								}
							}
						case '@':
							if !code37055(mem59-1, 4) {
								a := inputTemp[x]
								if a != 'H' {
									r = 1
								}
								if (a != 'T') && (a != 'C') && (a != 'S') {
									r = 1
								}
							}
						case '+':
							x = mem59
							x--
							a := inputTemp[x]
							if (a != 'E') && (a != 'I') && (a != 'Y') {
								r = 1
							}
						case ':':
							for code37055(mem59-1, 32) {
								mem59--
							}
							continue
						default:
							return false
						}
					}

					if r == 1 {
						matchFailed = true
						break
					}

					mem59 = x
				} else {
					break
				}

				if matchFailed {
					continue
				}
			}
			for {
				if !processRuleFlag {
					x = inputTempIndex + 1
					if inputTemp[x] == 'E' {
						if (tab36376[inputTemp[x+1]] & 128) != 0 {
							x++
							a := inputTemp[x]
							if a == 'L' {
								x++
								if inputTemp[x] != 'Y' {
									matchFailed = true
									break
								}
							} else if (a != 'R') && (a != 'S') && (a != 'D') && !match("FUL", inputTemp[x:]) {
								matchFailed = true
								break
							}
						}
					} else {
						if !match("ING", inputTemp[x:]) {
							matchFailed = true
							break
						}
						inputTempIndex = x
					}
				}
				processRuleFlag = false

				r = 0
				for {
					for {
						y = mem65ClosingBrace + 1
						if y == mem64EqualSignInRule {
							mem61InputPos = mem60InputMatchPos

							if debug {
								printRule(mem62)
							}

							for {
								mem57CurrentFlags = getRuleByte(mem62, y)
								a := mem57CurrentFlags & 127
								if a != '=' {
									mem56PhonemeOutpos++
									input[mem56PhonemeOutpos] = a
								}
								if (mem57CurrentFlags & 128) != 0 {
									break // Break out of the inner for loop
								}
								y++
							}
							break // Break out of the outer for loop
						}
						mem65ClosingBrace = y
						mem57CurrentFlags = getRuleByte(mem62, y)
						if (tab36376[mem57CurrentFlags] & 128) == 0 {
							break
						}
						if inputTemp[inputTempIndex+1] != mem57CurrentFlags {
							r = 1
							break
						}
						inputTempIndex++
					}

					if r == 0 {
						a := mem57CurrentFlags
						if a == '@' {
							if !code37055(inputTempIndex+1, 4) {
								a = inputTemp[x]
								if (a != 'R') && (a != 'T') && (a != 'C') && (a != 'S') {
									r = 1
								}
							} else {
								r = -2
							}
						} else if a == ':' {
							for code37055(inputTempIndex+1, 32) {
								inputTempIndex = x
							}
							r = -2
						} else {
							r = handleCh(a, inputTempIndex+1)
						}
					}

					if r == 1 {
						matchFailed = true
						break
					}
					if r == -2 {
						r = 0
						continue
					}
					if r == 0 {
						inputTempIndex = x
					}

					if r != 0 || y == mem64EqualSignInRule {
						break
					}

				}

				if matchFailed {
					break
				}

				if y == mem64EqualSignInRule {
					break
				}

				if mem57CurrentFlags != '%' {
					break
				}
			}

			if matchFailed {
				continue
			}
			break
		}
	}
}

func renderVoicedSample(hi uint16, off uint8, phase1 uint8) uint8 {
	for {
		bit := uint8(8)
		sample := sampleTable[hi+uint16(off)]

		for bit != 0 {
			if (sample & 128) != 0 {
				output(3, 26)
			} else {
				output(4, 6)
			}
			sample <<= 1
			bit--
		}

		off++
		phase1++

		if phase1 == 0 {
			break
		}
	}
	return off
}

func combineGlottalAndFormants(phase1, phase2, phase3, Y uint8) {
	var tmp uint32

	tmp = uint32(multtable[sinus[phase1]|amplitude1[Y]])
	tmp += uint32(multtable[sinus[phase2]|amplitude2[Y]])

	if tmp > 255 {
		tmp += 1
	} else {
		tmp += 0
	}

	tmp += uint32(multtable[rectangle[phase3]|amplitude3[Y]])
	tmp += 136
	tmp >>= 4 // Scale down to 0..15 range of C64 audio.

	output(0, uint8(tmp&0xf))
}

func renderSample(mem66OpenBrace *uint8, consonantFlag, mem49 uint8) {
	// mask low three bits and subtract 1 to get value to
	// convert 0 bits on unvoiced samples.
	hibyte := (consonantFlag & 7) - 1

	// determine which offset to use from table { 0x18, 0x1A, 0x17, 0x17, 0x17 }
	// T, S, Z                0          0x18
	// CH, J, SH, ZH          1          0x1A
	// P, F*, V, TH, DH       2          0x17
	// /H                     3          0x17
	// /X                     4          0x17

	hi := uint16(hibyte) * 256
	// voiced sample?
	pitchl := consonantFlag & 248
	if pitchl == 0 {
		// voiced phoneme: Z*, ZH, V*, DH
		pitchl = pitches[mem49] >> 4
		*mem66OpenBrace = renderVoicedSample(hi, *mem66OpenBrace, pitchl^255)
	} else {
		renderUnvoicedSample(hi, pitchl^255, tab48426[hibyte])
	}
}

func renderUnvoicedSample(hi uint16, off, mem53 uint8) {
	for {
		bit := uint8(8)
		sample := sampleTable[hi+uint16(off)]
		for bit != 0 {
			if (sample & 128) != 0 {
				output(2, 5)
			} else {
				output(1, mem53)
			}
			sample <<= 1
			bit--
		}
		off++
		if off == 0 {
			break
		}
	}
}

func getBufferLength() int {
	return bufferpos
}

func handleCh2(ch byte, mem int, inputTemp []byte) int {
	x = byte(mem)
	tmp := tab36376[inputTemp[mem]]
	if ch == ' ' {
		if tmp&128 != 0 {
			return 1
		}
	} else if ch == '#' {
		if tmp&64 == 0 {
			return 1
		}
	} else if ch == '.' {
		if tmp&8 == 0 {
			return 1
		}
	} else if ch == '^' {
		if tmp&32 == 0 {
			return 1
		}
	} else {
		return -1
	}
	return 0
}

func parser1() bool {
	var sign1 byte
	position := byte(0)
	srcpos := byte(0)

	for i := 0; i < 256; i++ {
		stress[i] = 0
	}

	for input[srcpos] != 155 { // 155 (\233) is end of line marker
		sign1 = input[srcpos]
		match := -1
		sign2 := input[srcpos+1]
		match = fullMatch(sign1, sign2)
		if match != -1 {
			// Matched both characters (no wildcards)
			phonemeindex[position] = byte(match)
			srcpos++ // Skip the second character of the input as we've matched it
		} else {
			match = wildMatch(sign1)
			if match != -1 {
				// Matched just the first character (with second character matching '*')
				phonemeindex[position] = byte(match)
			} else {
				// Should be a stress character. Search through the
				// stress table backwards.
				match = 8 // End of stress table. FIXME: Don't hardcode.
				for stressInputTable[match] != sign1 && match > 0 {
					match--
				}

				if match == 0 {
					return false // failure
				}

				stress[position-1] = byte(match) // Set stress for prior phoneme
			}
		}
		position++
		srcpos++
	}

	phonemeindex[position] = 255 // Mark end of phoneme list
	return true
}

func samMain() bool {
	initThings()

	if !parser1() {
		return false
	}
	if debug {
		printPhonemes(phonemeindex, phonemeLength, stress)
	}
	parser2()
	copyStress()
	setPhonemeLength()
	adjustLengths()
	code41240()
	for x := byte(0); phonemeindex[x] != 255; x++ {
		if phonemeindex[x] > 80 {
			phonemeindex[x] = 255
			break
		}
	}
	insertBreath()

	if debug {
		printPhonemes(phonemeindex, phonemeLength, stress)
	}
	prepareOutput()
	return true
}

func fullMatch(sign1, sign2 byte) int {
	for y := 0; y < 81; y++ {
		// GET FIRST CHARACTER AT POSITION Y IN signInputTable
		a := signInputTable1[y]

		if a == sign1 {
			a = signInputTable2[y]
			// NOT A SPECIAL AND MATCHES SECOND CHARACTER?
			if a != '*' && a == sign2 {
				return y
			}
		}
	}
	return -1
}

func wildMatch(sign1 byte) int {
	for y := 0; y < 81; y++ {
		if signInputTable2[y] == '*' {
			if signInputTable1[y] == sign1 {
				return y
			}
		}
	}
	return -1
}

func min(l, r int) int {
	if l < r {
		return l
	}
	return r
}

func createFrames() {
	x = byte(0)
	for i := 0; i < 256; i++ {
		// get the phoneme at the index
		phoneme := phonemeIndexOutput[i]

		// if terminal phoneme, exit the loop
		if phoneme == 255 {
			break
		}

		if phoneme == PHONEME_PERIOD {
			addInflection(RISING_INFLECTION, x)
		} else if phoneme == PHONEME_QUESTION {
			addInflection(FALLING_INFLECTION, x)
		}

		// get the stress amount (more stress = higher pitch)
		phase1 := tab47492[stressOutput[i]+1]

		// get number of frames to write
		phase2 := phonemeLengthOutput[i]

		// copy from the source to the frames list
		for phase2 != 0 {
			frequency1[x] = freq1data[phoneme]                       // F1 frequency
			frequency2[x] = freq2data[phoneme]                       // F2 frequency
			frequency3[x] = freq3data[phoneme]                       // F3 frequency
			amplitude1[x] = ampl1data[phoneme]                       // F1 amplitude
			amplitude2[x] = ampl2data[phoneme]                       // F2 amplitude
			amplitude3[x] = ampl3data[phoneme]                       // F3 amplitude
			sampledConsonantFlag[x] = sampledConsonantFlags[phoneme] // phoneme data for sampled consonants
			pitches[x] = pitch + phase1                              // pitch

			x++
			phase2--
		}
	}
}

func code37055(npos, mask byte) bool {
	x = npos
	result := tab36376[inputTemp[x]] & mask
	if result == 0 {
		return false
	} else {
		return true
	}
}

func createTransitions() uint8 {
	var mem49 uint8 = 0
	var pos uint8 = 0

	for {
		var next_rank uint8
		var rank uint8
		var speedcounter uint8
		var phase1 uint8
		var phase2 uint8
		var phase3 uint8
		var transition uint8

		phoneme := phonemeIndexOutput[pos]
		nextPhoneme := phonemeIndexOutput[pos+1]

		if nextPhoneme == 255 {
			break // 255 == end_token
		}

		// get the ranking of each phoneme
		next_rank = blendRank[nextPhoneme]
		rank = blendRank[phoneme]

		// compare the rank - lower rank value is stronger
		if rank == next_rank {
			// same rank, so use out blend lengths from each phoneme
			phase1 = outBlendLength[phoneme]
			phase2 = outBlendLength[nextPhoneme]
		} else if rank < next_rank {
			// next phoneme is stronger, so us its blend lengths
			phase1 = inBlendLength[nextPhoneme]
			phase2 = outBlendLength[nextPhoneme]
		} else {
			// current phoneme is stronger, so use its blend lengths
			// note the out/in are swapped
			phase1 = outBlendLength[phoneme]
			phase2 = inBlendLength[phoneme]
		}

		mem49 += phonemeLengthOutput[pos]

		speedcounter = mem49 + phase2
		phase3 = mem49 - phase1
		transition = phase1 + phase2 // total transition?

		if ((transition - 2) & 128) == 0 {
			table := uint8(169)
			interpolatePitch(pos, mem49, phase3)
			for table < 175 {
				// tables:
				// 168  pitches[]
				// 169  frequency1
				// 170  frequency2
				// 171  frequency3
				// 172  amplitude1
				// 173  amplitude2
				// 174  amplitude3

				value := int8(read(table, speedcounter)) - int8(read(table, phase3))
				interpolate(transition, table, phase3, value)
				table++
			}
		}
		pos++
	}

	// add the length of this phoneme
	return mem49 + phonemeLengthOutput[pos]
}

func getRuleByte(mem62 uint16, y byte) byte {
	address := int(mem62)
	if mem62 >= 37541 {
		address -= 37541
		return rules2[address+int(y)]
	}
	address -= 32000
	return rules[address+int(y)]
}

func read(p, y byte) byte {
	switch p {
	case 168:
		return pitches[y]
	case 169:
		return frequency1[y]
	case 170:
		return frequency2[y]
	case 171:
		return frequency3[y]
	case 172:
		return amplitude1[y]
	case 173:
		return amplitude2[y]
	case 174:
		return amplitude3[y]
	default:
		panic("Error reading from tables")
	}
}

func addInflection(inflection, pos byte) {
	end := pos

	if pos < 30 {
		pos = 0
	} else {
		pos -= 30
	}

	var a byte
	for pitches[pos] == 127 {
		pos++
	}
	a = pitches[pos]

	for pos != end {
		a += inflection
		pitches[pos] = a

		for {
			pos++
			if pos == end {
				break // Exit loop if we've reached the end
			}
			if pitches[pos] != 255 {
				break // Exit loop if we've found a non-255 pitch
			}
		}
	}
}

func assignPitchContour() {
	for i := 0; i < 256; i++ {
		pitches[i] -= (frequency1[i] >> 1)
	}
}

func changeRule(position, mem60InputMatchPos byte, descr string) {
	if debug {
		fmt.Printf("RULE: %s\n", descr)
	}
	phonemeindex[position] = 13 // rule
	insert(position+1, mem60InputMatchPos, 0, stress[position])
}

func change(pos, val byte, rule string) {
	if debug {
		fmt.Printf("RULE: %s\n", rule)
	}
	phonemeindex[pos] = val
}

func code41240() {
	pos := byte(0)
	for phonemeindex[pos] != 255 { // 255 == END
		index := phonemeindex[pos]
		if (flags[index] & FLAG_STOPCONS) != 0 {
			if (flags[index] & FLAG_PLOSIVE) != 0 {
				x := pos
				for phonemeindex[x+1] == 0 {
					x++ // Skip pause
				}
				a := phonemeindex[x+1]
				if a != 255 { // END
					if (flags[a]&8) != 0 || a == 36 || a == 37 {
						pos++
						continue
					}
				}
			}
			insert(pos+1, index+1, phonemeLengthTable[index+1], stress[pos])
			insert(pos+2, index+2, phonemeLengthTable[index+2], stress[pos])
			pos += 2
		}
		pos++
	}
}

func copyStress() {
	pos := byte(0)
	for phonemeindex[pos] != 255 { // 255 == END
		if (flags[phonemeindex[pos]] & 64) != 0 {
			y := phonemeindex[pos+1]
			if y != 255 && (flags[y]&128) != 0 {
				y = stress[pos+1]
				if y != 0 && (y&128) == 0 {
					stress[pos] = y + 1
				}
			}
		}
		pos++
	}
}

func drule(str string) {
	if debug {
		fmt.Printf("RULE: %s\n", str)
	}
}

func adjustLengths() {
	x := byte(0)
	for {
		index := phonemeindex[x]
		if index == 255 { // END
			break
		}

		// not punctuation?
		if (flags[index] & FLAG_PUNCT) == 0 {
			x++
			continue
		}
		loopIndex := x

		// Back up to the first vowel
		for x > 0 {
			x-- // Decrement X
			if flags[phonemeindex[x]]&FLAG_VOWEL != 0 {
				break // Exit the loop if a vowel is found
			}
		}

		if x == 0 {
			break
		}

		for x != loopIndex {
			index = phonemeindex[x]
			if (flags[index]&FLAG_FRICATIVE) == 0 || (flags[index]&FLAG_VOICED) != 0 {
				drulePre("Lengthen <FRICATIVE> or <VOICED> between <VOWEL> and <PUNCTUATION> by 1.5", x)
				a := phonemeLength[x]
				phonemeLength[x] = (a >> 1) + a + 1
				drulePost(x)
			}
			x++
		}
		x++
	}

	loopIndex := byte(0)
	for {
		index := phonemeindex[loopIndex]
		if index == 255 { // END
			break
		}

		if (flags[index] & FLAG_VOWEL) != 0 {
			x := loopIndex
			index = phonemeindex[loopIndex+1]
			if (flags[index] & FLAG_CONSONANT) == 0 {
				if index == 18 || index == 19 { // 'RX', 'LX'
					index = phonemeindex[loopIndex+2]
					if (flags[index] & FLAG_CONSONANT) != 0 {
						drulePre("<VOWEL> <RX | LX> <CONSONANT> - decrease length of vowel by 1", loopIndex)
						phonemeLength[loopIndex]--
						drulePost(loopIndex)
					}
				}
			} else {
				var flag uint16
				if index == 255 { // END
					flag = 65
				} else {
					flag = flags[index]
				}

				if (flag & FLAG_VOICED) == 0 {
					if (flag & FLAG_PLOSIVE) != 0 {
						drulePre("<VOWEL> <UNVOICED PLOSIVE> - decrease vowel by 1/8th", loopIndex)
						phonemeLength[loopIndex] -= (phonemeLength[loopIndex] >> 3)
						drulePost(loopIndex)
					}
				} else {
					drulePre("<VOWEL> <VOICED CONSONANT> - increase vowel by 1/2 + 1", x)
					a := phonemeLength[loopIndex]
					phonemeLength[loopIndex] = (a >> 2) + a + 1 // 5/4*A + 1
					drulePost(loopIndex)
				}
			}
		} else if (flags[index] & FLAG_NASAL) != 0 {
			x := loopIndex + 1
			index = phonemeindex[x]
			if index != 255 && (flags[index]&FLAG_STOPCONS) != 0 {
				drulePre("<NASAL> <STOP CONSONANT> - set nasal = 5, consonant = 6", x)
				phonemeLength[x] = 6
				phonemeLength[x-1] = 5
				drulePost(x)
			}
		} else if (flags[index] & FLAG_STOPCONS) != 0 {
			x := loopIndex + 1
			for phonemeindex[x] == 0 {
				x++
			}

			index = phonemeindex[x]
			if index != 255 && (flags[index]&FLAG_STOPCONS) != 0 {
				drulePre("<STOP CONSONANT> {optional silence} <STOP CONSONANT> - shorten both to 1/2 + 1", x)
				phonemeLength[x] = (phonemeLength[x] >> 1) + 1
				phonemeLength[loopIndex] = (phonemeLength[loopIndex] >> 1) + 1
				drulePost(x)
				x = loopIndex
			}
		} else if (flags[index] & FLAG_LIQUIC) != 0 {
			index = phonemeindex[loopIndex-1]
			if (flags[index] & FLAG_STOPCONS) != 0 {
				drulePre("<LIQUID CONSONANT> <DIPTHONG> - decrease by 2", loopIndex)
				phonemeLength[loopIndex] -= 2
				drulePost(loopIndex)
			}
		}

		loopIndex++
	}
}

func enableSingmode() {
	singmode = true
}

func insertBreath() {
	mem54 := byte(255)
	len := byte(0)
	pos := byte(0)

	for {
		index := phonemeindex[pos]
		if index == 255 { // END
			break
		}

		len += phonemeLength[pos]
		if len < 232 {
			if index == BREAK {
				// Do nothing
			} else if (flags[index] & FLAG_PUNCT) == 0 {
				if index == 0 {
					mem54 = pos
				}
			} else {
				len = 0
				insert(pos+1, BREAK, 0, 0)
			}
		} else {
			pos = mem54
			phonemeindex[pos] = 31 // 'Q*' glottal stop
			phonemeLength[pos] = 4
			stress[pos] = 0

			len = 0
			insert(pos+1, BREAK, 0, 0)
		}
		pos++
	}
}

func insert(position, mem60InputMatchPos, mem59, mem58Variant byte) {
	for i := 253; i >= int(position); i-- {
		phonemeindex[i+1] = phonemeindex[i]
		phonemeLength[i+1] = phonemeLength[i]
		stress[i+1] = stress[i]
	}

	phonemeindex[position] = mem60InputMatchPos
	phonemeLength[position] = mem59
	stress[position] = mem58Variant
}

func interpolate(width, table, frame byte, mem53 int8) {
	sign := mem53 < 0
	remainder := byte(abs(int(mem53))) % width
	div := uint8(int(mem53) / int(width))
	error := byte(0)
	pos := width
	val := read(table, frame) + div

	pos--
	for pos > 0 {
		error += remainder
		if error >= width { // accumulated a whole integer error, so adjust output
			error -= width
			if sign {
				val--
			} else if val != 0 {
				val++ // if input is 0, we always leave it alone
			}
		}
		frame++
		write(table, frame, val) // Write updated value back to next frame.
		val += div
		pos--
	}
}

func trans(a, b byte) byte {
	return byte(((uint16(a) * uint16(b)) >> 8) << 1)
}

func interpolatePitch(pos, mem49, phase3 byte) {
	// half the width of the current and next phoneme
	curWidth := phonemeLengthOutput[pos] / 2
	nextWidth := phonemeLengthOutput[pos+1] / 2

	// sum the values
	width := curWidth + nextWidth
	pitch := int8(pitches[nextWidth+mem49]) - int8(pitches[mem49-curWidth])
	interpolate(width, 168, phase3, pitch)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func prepareOutput() {
	srcpos := byte(0)
	destpos := byte(0)

	for {
		a := phonemeindex[srcpos]
		phonemeIndexOutput[destpos] = a
		switch a {
		case 255:
			render()
			return
		case 254:
			phonemeIndexOutput[destpos] = 255
			render()
			destpos = 0
		case 0:
			// Do nothing
		default:
			phonemeLengthOutput[destpos] = phonemeLength[srcpos]
			stressOutput[destpos] = stress[srcpos]
			destpos++
		}
		srcpos++
	}
}

func printOutput(flag, f1, f2, f3, a1, a2, a3, p []byte) {
	fmt.Println("===========================================")
	fmt.Println("Final data for speech output:")
	fmt.Println()
	fmt.Println(" flags ampl1 freq1 ampl2 freq2 ampl3 freq3 pitch")
	fmt.Println("------------------------------------------------")
	for i := 0; i < 255; i++ {
		fmt.Printf("%5d %5d %5d %5d %5d %5d %5d %5d\n", flag[i], a1[i], f1[i], a2[i], f2[i], a3[i], f3[i], p[i])
	}
	fmt.Println("===========================================")
}

func printPhonemes(phonemeIndex, phonemeLength, stress []byte) {
	fmt.Println("===========================================")
	fmt.Println("Internal Phoneme presentation:")
	fmt.Println()
	fmt.Println(" idx    phoneme  length  stress")
	fmt.Println("------------------------------")
	i := 0
	for phonemeIndex[i] != 255 && i < 255 {
		if phonemeIndex[i] < 81 {
			fmt.Printf(" %3d      %c%c      %3d       %d\n", phonemeIndex[i],
				signInputTable1[phonemeIndex[i]], signInputTable2[phonemeIndex[i]],
				phonemeLength[i], stress[i])
		} else {
			fmt.Printf(" %3d      ??      %3d       %d\n", phonemeIndex[i],
				phonemeLength[i], stress[i])
		}
		i++
	}
	fmt.Println("===========================================")
	fmt.Println()
}

func outputSound() {
	// This function might need to be adapted based on your audio output method in Go
	fmt.Println("Audio output not implemented")
}

func parser2() {
	pos := byte(0)
	for p := phonemeindex[pos]; p != 255; p = phonemeindex[pos] {
		if debug {
			fmt.Printf("%d: %c%c\n", pos, signInputTable1[p], signInputTable2[p])
		}

		if p == 0 {
			pos++
			continue
		}

		pf := flags[p]
		prior := phonemeindex[pos-1]

		if (pf & FLAG_DIPTHONG) != 0 {
			ruleDipthong(p, byte(pf), pos)
		} else if p == 78 {
			changeRule(pos, 24, "UL -> AX L") // Example: MEDDLE
		} else if p == 79 {
			changeRule(pos, 27, "UM -> AX M") // Example: ASTRONOMY
		} else if p == 80 {
			changeRule(pos, 28, "UN -> AX N") // Example: FUNCTION
		} else if (pf&FLAG_VOWEL) != 0 && stress[pos] != 0 {
			// RULE: <STRESSED VOWEL> <SILENCE> <STRESSED VOWEL> -> <STRESSED VOWEL> <SILENCE> Q <VOWEL>
			// EXAMPLE: AWAY EIGHT
			if phonemeindex[pos+1] == 0 { // If following phoneme is a pause, get next
				p2 := phonemeindex[pos+2]
				if p2 != 255 && (flags[p2]&FLAG_VOWEL) != 0 && stress[pos+2] != 0 {
					drule("Insert glottal stop between two stressed vowels with space between them")
					insert(pos+2, 31, 0, 0) // 31 = 'Q'
				}
			}
		} else if p == 23 { // 'R'
			if prior == 69 { // 'T'
				change(pos-1, 42, "T R -> CH R") // Example: TRACK
			} else if prior == 57 { // 'D'
				change(pos-1, 44, "D R -> J R") // Example: DRY
			} else if (flags[prior] & FLAG_VOWEL) != 0 {
				change(pos, 18, "<VOWEL> R -> <VOWEL> RX") // Example: ART
			}
		} else if p == 24 && (flags[prior]&FLAG_VOWEL) != 0 {
			change(pos, 19, "<VOWEL> L -> <VOWEL> LX") // Example: ALL
		} else if prior == 60 && p == 32 { // 'G' 'S'
			change(pos, 38, "G S -> G Z")
		} else if p == 60 { // 'G'
			ruleG(pos)
		} else {
			if p == 72 { // 'K'
				y := phonemeindex[pos+1]
				if y == 255 || (flags[y]&FLAG_DIP_YX) == 0 {
					change(pos, 75, "K <VOWEL OR DIPTHONG NOT ENDING WITH IY> -> KX <VOWEL OR DIPTHONG NOT ENDING WITH IY>")
					p = 75
					pf = flags[p]
				}
			}

			// Replace with softer version?
			if (flags[p]&FLAG_PLOSIVE) != 0 && prior == 32 { // 'S'
				// RULE: S P -> S B
				//       S T -> S D
				//       S K -> S G
				//       S KX -> S GX
				// Examples: SPY, STY, SKY, SCOWL
				if debug {
					fmt.Printf("RULE: S* %c%c -> S* %c%c\n", signInputTable1[p], signInputTable2[p], signInputTable1[p-12], signInputTable2[p-12])
				}
				phonemeindex[pos] = p - 12
			} else if (pf & FLAG_PLOSIVE) == 0 {
				p = phonemeindex[pos]
				if p == 53 {
					ruleAlveolarUw(pos) // Example: NEW, DEW, SUE, ZOO, THOO, TOO
				} else if p == 42 {
					ruleCh(pos) // Example: CHEW
				} else if p == 44 {
					ruleJ(pos) // Example: JAY
				}
			}

			if p == 69 || p == 57 { // 'T', 'D'
				// RULE: Soften T following vowel
				// NOTE: This rule fails for cases such as "ODD"
				//       <UNSTRESSED VOWEL> T <PAUSE> -> <UNSTRESSED VOWEL> DX <PAUSE>
				//       <UNSTRESSED VOWEL> D <PAUSE>  -> <UNSTRESSED VOWEL> DX <PAUSE>
				// Example: PARTY, TARDY
				if (flags[phonemeindex[pos-1]] & FLAG_VOWEL) != 0 {
					p = phonemeindex[pos+1]
					if p == 0 {
						p = phonemeindex[pos+2]
					}
					if (flags[p]&FLAG_VOWEL) != 0 && stress[pos+1] == 0 {
						change(pos, 30, "Soften T or D following vowel or ER and preceding a pause -> DX")
					}
				}
			}
		}
		pos++
	}
}

func processFrames(mem48 byte) {
	speedcounter := byte(72)
	phase1 := byte(0)
	phase2 := byte(0)
	phase3 := byte(0)
	mem66OpenBrace := byte(0)

	y := byte(0)

	glottalPulse := pitches[0]
	mem38 := glottalPulse - (glottalPulse >> 2) // mem44 * 0.75

	for mem48 != 0 {
		flags := sampledConsonantFlag[y]

		// unvoiced sampled phoneme?
		if (flags & 248) != 0 {
			renderSample(&mem66OpenBrace, flags, y)
			// skip ahead two in the phoneme buffer
			y += 2
			mem48 -= 2
			speedcounter = speed
		} else {
			combineGlottalAndFormants(phase1, phase2, phase3, y)

			speedcounter--
			if speedcounter == 0 {
				y++ // go to next amplitude
				// decrement the frame count
				mem48--
				if mem48 == 0 {
					return
				}
				speedcounter = speed
			}

			glottalPulse--

			if glottalPulse != 0 {
				// not finished with a glottal pulse
				mem38--
				// within the first 75% of the glottal pulse?
				// is the count non-zero and the sampled flag is zero?
				if mem38 != 0 || flags == 0 {
					// reset the phase of the formants to match the pulse
					phase1 += frequency1[y]
					phase2 += frequency2[y]
					phase3 += frequency3[y]
					continue
				}

				// voiced sampled phonemes interleave the sample with the
				// glottal pulse. The sample flag is non-zero, so render
				// the sample for the phoneme.
				renderSample(&mem66OpenBrace, flags, y)
			}
		}

		glottalPulse = pitches[y]
		mem38 = glottalPulse - (glottalPulse >> 2) // mem44 * 0.75

		// reset the formant wave generators to keep them in
		// sync with the glottal pulse
		phase1 = 0
		phase2 = 0
		phase3 = 0
	}
}

func render() {
	if phonemeIndexOutput[0] == 255 {
		return // exit if no data
	}

	createFrames()
	t := createTransitions()

	if !singmode {
		assignPitchContour()
	}
	rescaleAmplitude()

	if debug {
		printOutput(sampledConsonantFlag, frequency1, frequency2, frequency3, amplitude1, amplitude2, amplitude3, pitches)
	}

	processFrames(t)
}

// RESCALE AMPLITUDE
//
// Rescale volume from a linear scale to decibels.
func rescaleAmplitude() {
	for i := 255; i >= 0; i-- {
		amplitude1[i] = amplitudeRescale[amplitude1[i]]
		amplitude2[i] = amplitudeRescale[amplitude2[i]]
		amplitude3[i] = amplitudeRescale[amplitude3[i]]
	}
}

func output(index int, A byte) {
	var oldtimetableindex int
	bufferpos += timetable[oldtimetableindex][index]
	oldtimetableindex = index
	// write a little bit in advance
	for k := 0; k < 5; k++ {
		buffer[bufferpos/50+k] = (A & 15) * 16
	}
}

func printUsage() {
	fmt.Println("usage: sam [options] Word1 Word2 ....")
	fmt.Println("options")
	fmt.Println("  -phonetic       enters phonetic mode. (see below)")
	fmt.Println("  -pitch number       set pitch value (default=64)")
	fmt.Println("  -speed number       set speed value (default=72)")
	fmt.Println("  -throat number      set throat value (default=128)")
	fmt.Println("  -mouth number       set mouth value (default=128)")
	fmt.Println("  -wav filename       output to wav instead of libsdl")
	fmt.Println("  -sing           special treatment of pitch")
	fmt.Println("  -debug          print additional debug messages")
	fmt.Println("")
	fmt.Println("     VOWELS                            VOICED CONSONANTS")
	fmt.Println("IY           f(ee)t                    R        red")
	fmt.Println("IH           p(i)n                     L        allow")
	fmt.Println("EH           beg                       W        away")
	fmt.Println("AE           Sam                       W        whale")
	fmt.Println("AA           pot                       Y        you")
	fmt.Println("AH           b(u)dget                  M        Sam")
	fmt.Println("AO           t(al)k                    N        man")
	fmt.Println("OH           cone                      NX       so(ng)")
	fmt.Println("UH           book                      B        bad")
	fmt.Println("UX           l(oo)t                    D        dog")
	fmt.Println("ER           bird                      G        again")
	fmt.Println("AX           gall(o)n                  J        judge")
	fmt.Println("IX           dig(i)t                   Z        zoo")
	fmt.Println("                                       ZH       plea(s)ure")
	fmt.Println("   DIPHTHONGS                          V        seven")
	fmt.Println("EY           m(a)de                    DH       (th)en")
	fmt.Println("AY           h(igh)")
	fmt.Println("OY           boy")
	fmt.Println("AW           h(ow)                     UNVOICED CONSONANTS")
	fmt.Println("OW           slow                      S         Sam")
	fmt.Println("UW           crew                      Sh        fish")
	fmt.Println("                                       F         fish")
	fmt.Println("                                       TH        thin")
	fmt.Println(" SPECIAL PHONEMES                      P         poke")
	fmt.Println("UL           sett(le) (=AXL)           T         talk")
	fmt.Println("UM           astron(omy) (=AXM)        K         cake")
	fmt.Println("UN           functi(on) (=AXN)         CH        speech")
	fmt.Println("Q            kitt-en (glottal stop)    /H        a(h)ead")
}

func ruleAlveolarUw(x byte) {
	// ALVEOLAR flag set?
	if (flags[phonemeindex[x-1]] & FLAG_ALVEOLAR) != 0 {
		drule("<ALVEOLAR> UW -> <ALVEOLAR> UX")
		phonemeindex[x] = 16
	}
}

func ruleCh(x byte) {
	drule("CH -> CH CH+1")
	insert(x+1, 43, 0, stress[x])
}

func ruleDipthong(p, pf byte, pos byte) {
	// <DIPTHONG ENDING WITH WX> -> <DIPTHONG ENDING WITH WX> WX
	// <DIPTHONG NOT ENDING WITH WX> -> <DIPTHONG NOT ENDING WITH WX> YX
	// Example: OIL, COW

	// If ends with IY, use YX, else use WX
	var a byte
	if (pf & FLAG_DIP_YX) != 0 {
		a = 21
	} else {
		a = 20
	}
	// Insert at WX or YX following, copying the stress
	if a == 20 {
		drule("insert WX following dipthong NOT ending in IY sound")
	} else if a == 21 {
		drule("insert YX following dipthong ending in IY sound")
	}
	insert(pos+1, a, 0, stress[pos])

	if p == 53 {
		ruleAlveolarUw(pos) // Example: NEW, DEW, SUE, ZOO, THOO, TOO
	} else if p == 42 {
		ruleCh(pos) // Example: CHEW
	} else if p == 44 {
		ruleJ(pos) // Example: JAY
	}
}

func ruleG(pos byte) {
	// G <VOWEL OR DIPTHONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPTHONG NOT ENDING WITH IY>
	// Example: GO

	index := phonemeindex[pos+1]

	// If dipthong ending with YX, move continue processing next phoneme
	if index != 255 && ((flags[index] & FLAG_DIP_YX) == 0) {
		// replace G with GX and continue processing next phoneme
		drule("G <VOWEL OR DIPTHONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPTHONG NOT ENDING WITH IY>")
		phonemeindex[pos] = 63 // 'GX'
	}
}

func ruleJ(x byte) {
	drule("J -> J J+1")
	insert(x+1, 45, 0, stress[x])
}

func setInput(input []byte) []byte {
	result := make([]byte, 256)
	result[0] = ' '

	for i := 0; i < len(input) && i < 255; i++ {
		char := input[i] & 127
		if char >= 112 {
			char = char & 95
		} else if char >= 96 {
			char = char & 79
		}
		result[i+1] = char
	}
	result[255] = 27
	return result
}

func setMouthThroat(mouth, throat byte) {
	mouthFormants5_29 := []byte{
		0, 0, 0, 0, 0, 10, 14, 19, 24, 27, 23, 21, 16, 20, 14,
		18, 14, 18, 18, 16, 13, 15, 11, 18, 14, 11, 9, 6, 6, 6,
	}
	throatFormants5_29 := []byte{
		255, 255, 255, 255, 255, 84, 73, 67, 63, 40, 44, 31, 37, 45, 73,
		49, 36, 30, 51, 37, 29, 69, 24, 50, 30, 24, 83, 46, 54, 86,
	}
	mouthFormants48_53 := []byte{19, 27, 21, 27, 18, 13}
	throatFormants48_53 := []byte{72, 39, 31, 43, 30, 34}

	pos := 5
	for pos < 30 {
		initialFrequency := mouthFormants5_29[pos]
		if initialFrequency != 0 {
			freq1data[pos] = trans(mouth, initialFrequency)
		}

		initialFrequency = throatFormants5_29[pos]
		if initialFrequency != 0 {
			freq2data[pos] = trans(throat, initialFrequency)
		}
		pos++
	}

	pos = 0
	for pos < 6 {
		initialFrequency := mouthFormants48_53[pos]
		freq1data[pos+48] = trans(mouth, initialFrequency)

		initialFrequency = throatFormants48_53[pos]
		freq2data[pos+48] = trans(throat, initialFrequency)
		pos++
	}
}

func setMouth(mouth byte) {
	setMouthThroat(mouth, 128)
}

func setPhonemeLength() {
	position := byte(0)
	for phonemeindex[position] != 255 {
		a := stress[position]
		if a == 0 || (a&128) != 0 {
			phonemeLength[position] = phonemeLengthTable[phonemeindex[position]]
		} else {
			phonemeLength[position] = phonemeStressedLengthTable[phonemeindex[position]]
		}
		position++
	}
}

func setPitch(pitchSource byte) {
	pitch = pitchSource
}

func setSpeed(speedSource byte) {
	speed = speedSource
}

func setThroat(throat byte) {
	setMouthThroat(128, throat)
}

func write(p, y, value byte) {
	switch p {
	case 168:
		pitches[y] = value
	case 169:
		frequency1[y] = value
	case 170:
		frequency2[y] = value
	case 171:
		frequency3[y] = value
	case 172:
		amplitude1[y] = value
	case 173:
		amplitude2[y] = value
	case 174:
		amplitude3[y] = value
	default:
		panic("Error writing to tables")
	}
}

func writeWav(filename string, buffer []byte, bufferLength int) error {
	file, err := fopenS(filename, "wb")
	if err != nil {
		return fmt.Errorf("failed to open file: %v", err)
	}
	defer file.Close()

	// RIFF header
	if _, err := file.Write([]byte("RIFF")); err != nil {
		return fmt.Errorf("failed to write RIFF header: %v", err)
	}

	fileSize := uint32(bufferLength + 12 + 16 + 8 - 8)
	if err := binary.Write(file, binary.LittleEndian, fileSize); err != nil {
		return fmt.Errorf("failed to write file size: %v", err)
	}

	if _, err := file.Write([]byte("WAVE")); err != nil {
		return fmt.Errorf("failed to write WAVE header: %v", err)
	}

	// format chunk
	if _, err := file.Write([]byte("fmt ")); err != nil {
		return fmt.Errorf("failed to write format chunk header: %v", err)
	}

	fmtLength := uint32(16)
	if err := binary.Write(file, binary.LittleEndian, fmtLength); err != nil {
		return fmt.Errorf("failed to write format chunk length: %v", err)
	}

	format := uint16(1) // PCM
	if err := binary.Write(file, binary.LittleEndian, format); err != nil {
		return fmt.Errorf("failed to write audio format: %v", err)
	}

	channels := uint16(1)
	if err := binary.Write(file, binary.LittleEndian, channels); err != nil {
		return fmt.Errorf("failed to write number of channels: %v", err)
	}

	sampleRate := uint32(22050)
	if err := binary.Write(file, binary.LittleEndian, sampleRate); err != nil {
		return fmt.Errorf("failed to write sample rate: %v", err)
	}

	if err := binary.Write(file, binary.LittleEndian, sampleRate); err != nil { // bytes/second
		return fmt.Errorf("failed to write byte rate: %v", err)
	}

	blockAlign := uint16(1)
	if err := binary.Write(file, binary.LittleEndian, blockAlign); err != nil {
		return fmt.Errorf("failed to write block align: %v", err)
	}

	bitsPerSample := uint16(8)
	if err := binary.Write(file, binary.LittleEndian, bitsPerSample); err != nil {
		return fmt.Errorf("failed to write bits per sample: %v", err)
	}

	// data chunk
	if _, err := file.Write([]byte("data")); err != nil {
		return fmt.Errorf("failed to write data chunk header: %v", err)
	}

	if err := binary.Write(file, binary.LittleEndian, uint32(bufferLength)); err != nil {
		return fmt.Errorf("failed to write data length: %v", err)
	}

	if _, err := file.Write(buffer); err != nil {
		return fmt.Errorf("failed to write audio data: %v", err)
	}

	return nil
}

func main() {
	var phonetic bool
	var wavFilename string
	input = make([]byte, 256)

	if len(os.Args) <= 1 {
		printUsage()
		os.Exit(1)
	}

	i := 1
	for i < len(os.Args) {
		if os.Args[i][0] != '-' {
			strcatS(input, 256, strings.ToUpper(os.Args[i]+" "))
		} else {
			switch os.Args[i][1:] {
			case "wav":
				if i+1 < len(os.Args) {
					wavFilename = os.Args[i+1]
					i++
				}
			case "sing":
				enableSingmode()
			case "phonetic":
				phonetic = true
			case "debug":
				debug = true
			case "pitch":
				if i+1 < len(os.Args) {
					pitch, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setPitch(byte(min(pitch, 255)))
					}
					i++
				}
			case "speed":
				if i+1 < len(os.Args) {
					speed, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setSpeed(byte(min(speed, 255)))
					}
					i++
				}
			case "mouth":
				if i+1 < len(os.Args) {
					mouth, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setMouth(byte(min(mouth, 255)))
					}
					i++
				}
			case "throat":
				if i+1 < len(os.Args) {
					throat, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setThroat(byte(min(throat, 255)))
					}
					i++
				}
			default:
				printUsage()
				os.Exit(1)
			}
		}
		i++
	}

	if debug {
		if phonetic {
			fmt.Printf("phonetic input: %s\n", string(input))
		} else {
			fmt.Printf("text input: %s\n", string(input))
		}
	}

	if !phonetic {
		strcatS(input, 256, "[")
		if !textToPhonemes(input) {
			os.Exit(1)
		}
		if debug {
			fmt.Printf("phonetic input: %s\n", string(input))
		}
	} else {
		strcatS(input, 256, "\x9b")
	}

	setInput(input)
	if !samMain() {
		printUsage()
		os.Exit(1)
	}
	if wavFilename != "" {
		writeWav(wavFilename, getBuffer(), getBufferLength()/50)
	} else {
		outputSound()
	}
}

func getBuffer() []byte {
	return buffer
}

func strcatS(dest []byte, size int, str string) {
	destLen := len(strings.TrimRight(string(dest), "\x00"))
	if destLen >= size-1 {
		return
	}
	strLen := len(str)
	if destLen+strLen >= size {
		strLen = size - destLen - 1
	}
	copy(dest[destLen:], str[:strLen])
	dest[destLen+strLen] = 0
}

func initThings() {
	setMouthThroat(mouth, throat)

	bufferpos = 0
	// TODO: check for free memory, 10 seconds of output should be more than enough
	buffer = make([]byte, 22050*10)

	for i := 0; i < 256; i++ {
		stress[i] = 0
		phonemeLength[i] = 0
	}

	for i := 0; i < 60; i++ {
		phonemeIndexOutput[i] = 0
		stressOutput[i] = 0
		phonemeLengthOutput[i] = 0
	}
	phonemeindex[255] = 255 // to prevent buffer overflow

	// ML : changed from 32 to 255 to stop freezing with long inputs
}

func drulePost(x byte) {
	if debug {
		fmt.Println("POST")
		fmt.Printf("phoneme %d (%c%c) length %d\n", x, signInputTable1[phonemeindex[x]],
			signInputTable2[phonemeindex[x]], phonemeLength[x])
	}
}

func drulePre(descr string, x byte) {
	drule(descr)
	if debug {
		fmt.Println("PRE")
		fmt.Printf("phoneme %d (%c%c) length %d\n", x, signInputTable1[phonemeindex[x]],
			signInputTable2[phonemeindex[x]], phonemeLength[x])
	}
}

// Helper function to parse file mode
func parseMode(mode string) int {
	var flag int
	switch mode {
	case "r":
		flag = os.O_RDONLY
	case "w":
		flag = os.O_WRONLY | os.O_CREATE | os.O_TRUNC
	case "a":
		flag = os.O_WRONLY | os.O_CREATE | os.O_APPEND
	default:
		flag = os.O_RDONLY
	}
	return flag
}

func fopenS(filename, mode string) (*os.File, error) {
	return os.OpenFile(filename, parseMode(mode), 0666)
}

func match(str string, inputTemp []byte) bool {
	for i := 0; i < len(str); i++ {
		if inputTemp[i] != str[i] {
			return false
		}
	}
	return true
}

func printRule(offset uint16) {
	i := byte(1)
	var a byte
	fmt.Print("Applying rule: ")
	for {
		a = getRuleByte(offset, i)
		if (a & 127) == '=' {
			fmt.Print(" -> ")
		} else {
			fmt.Printf("%c", a&127)
		}
		i++
		if (a & 128) != 0 {
			break
		}
	}
	fmt.Println()
}

func handleCh(ch, mem byte) int {
	x = mem
	tmp := tab36376[inputTemp[mem]]
	if ch == ' ' {
		if (tmp & 128) != 0 {
			return 1
		}
	} else if ch == '#' {
		if (tmp & 64) == 0 {
			return 1
		}
	} else if ch == '.' {
		if (tmp & 8) == 0 {
			return 1
		}
	} else if ch == '&' {
		if (tmp & 16) == 0 {
			if inputTemp[mem] != 72 {
				return 1
			}
			mem++
		}
	} else if ch == '^' {
		if (tmp & 32) == 0 {
			return 1
		}
	} else if ch == '+' {
		ch = inputTemp[mem]
		if ch != 69 && ch != 73 && ch != 89 {
			return 1
		}
	} else {
		return -1
	}
	return 0
}
