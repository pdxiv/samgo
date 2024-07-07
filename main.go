package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/ebitengine/oto/v3"
)

func initAudio(audioState *AudioState) error {
	audioState.SampleRate = SampleRate
	audioState.NumChannels = SampleChannels

	var err error

	audioState.OtoCtx, _, err = oto.NewContext(&oto.NewContextOptions{
		SampleRate:   audioState.SampleRate,
		ChannelCount: audioState.NumChannels,
		Format:       oto.FormatUnsignedInt8,
	})
	if err != nil {
		return fmt.Errorf("oto.NewContext failed: %v", err)
	}
	return nil
}

func convertAudioFormat(input []byte) []byte {
	output := make([]byte, len(input))
	for i, sample := range input {
		output[i] = sample ^ 0x80 // Convert unsigned 8-bit to signed 8-bit
	}
	return output
}

func playAudio(audioState *AudioState, buffer []byte, bufferLength int) error {
	if audioState.OtoCtx == nil {
		return fmt.Errorf("audio context not initialized")
	}

	// Convert audio format
	convertedBuffer := convertAudioFormat(buffer[:bufferLength])

	player := audioState.OtoCtx.NewPlayer(bytes.NewReader(convertedBuffer))
	defer player.Close()

	player.Play()

	// Wait for the audio to finish playing
	for player.IsPlaying() {
		time.Sleep(10 * time.Millisecond)
	}

	return nil
}

func textToPhonemes(samState *SamState, input []byte) bool {
	inputState := &samState.Input
	samConfig := &samState.Config

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

	inputState.InputTemp = make([]byte, 256)
	inputState.InputTemp[0] = ' '

	// secure copy of input because input will be overwritten by phonemes
	samState.X = byte(0)
	for samState.X < 255 {
		a := input[samState.X] & 127
		if a >= 112 {
			a &= 95
		} else if a >= 96 {
			a &= 79
		}
		samState.X++
		inputState.InputTemp[samState.X] = a
	}
	inputState.InputTemp[255] = 27
	mem56PhonemeOutpos, mem61InputPos = 255, 255

	for {
		for {
			for {
				mem61InputPos++
				samState.X = mem61InputPos
				mem64EqualSignInRule = inputState.InputTemp[samState.X]
				if mem64EqualSignInRule == '[' {
					mem56PhonemeOutpos++
					samState.X = mem56PhonemeOutpos
					input[samState.X] = 155
					return true
				}

				if mem64EqualSignInRule != '.' {
					break
				}
				samState.X++
				if (tab36376[inputState.InputTemp[samState.X]] & 1) != 0 {
					break
				}
				mem56PhonemeOutpos++
				samState.X = mem56PhonemeOutpos
				input[samState.X] = '.'
			}
			mem57CurrentFlags = tab36376[mem64EqualSignInRule]
			if (mem57CurrentFlags & 2) != 0 {
				mem62 = 37541
				break
			}

			if mem57CurrentFlags != 0 {
				break
			}
			inputState.InputTemp[samState.X] = ' '
			mem56PhonemeOutpos++
			samState.X = mem56PhonemeOutpos
			if samState.X > 120 {
				input[samState.X] = 155
				return true
			}
			input[samState.X] = 32
		}

		if (mem57CurrentFlags & 2) == 0 {
			if (mem57CurrentFlags & 128) == 0 {
				return false
			}

			// go to the right rules for this character.
			samState.X = mem64EqualSignInRule - 'A'
			mem62 = uint16(tab37489[samState.X]) | (uint16(tab37515[samState.X]) << 8)
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
			samState.X = mem60InputMatchPos
			// compare the string within the bracket
			y = mem66OpenBrace + 1

			matchFailed := false
			for {
				if getRuleByte(mem62, y) != inputState.InputTemp[samState.X] {
					matchFailed = true
					break
				}
				y++
				if y == mem65ClosingBrace {
					break
				}
				samState.X++
				mem60InputMatchPos = samState.X
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
					samState.X = mem57CurrentFlags & 127
					if (tab36376[samState.X] & 128) == 0 {
						break
					}
					if inputState.InputTemp[mem59-1] != mem57CurrentFlags {
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

					r = handleCh2(samState, ch, int(mem59-1), inputState.InputTemp)
					if r == -1 {
						switch ch {
						case '&':
							if !code37055(samState, mem59-1, 16) {
								if inputState.InputTemp[samState.X] != 'H' {
									r = 1
								} else {
									samState.X--
									a := inputState.InputTemp[samState.X]
									if (a != 'C') && (a != 'S') {
										r = 1
									}
								}
							}
						case '@':
							if !code37055(samState, mem59-1, 4) {
								a := inputState.InputTemp[samState.X]
								if a != 'H' {
									r = 1
								}
								if (a != 'T') && (a != 'C') && (a != 'S') {
									r = 1
								}
							}
						case '+':
							samState.X = mem59
							samState.X--
							a := inputState.InputTemp[samState.X]
							if (a != 'E') && (a != 'I') && (a != 'Y') {
								r = 1
							}
						case ':':
							for code37055(samState, mem59-1, 32) {
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

					mem59 = samState.X
				} else {
					break
				}

				if matchFailed {
					continue
				}
			}
			for {
				if !processRuleFlag {
					samState.X = inputTempIndex + 1
					if inputState.InputTemp[samState.X] == 'E' {
						if (tab36376[inputState.InputTemp[samState.X+1]] & 128) != 0 {
							samState.X++
							a := inputState.InputTemp[samState.X]
							if a == 'L' {
								samState.X++
								if inputState.InputTemp[samState.X] != 'Y' {
									matchFailed = true
									break
								}
							} else if (a != 'R') && (a != 'S') && (a != 'D') && !match("FUL", inputState.InputTemp[samState.X:]) {
								matchFailed = true
								break
							}
						}
					} else {
						if !match("ING", inputState.InputTemp[samState.X:]) {
							matchFailed = true
							break
						}
						inputTempIndex = samState.X
					}
				}
				processRuleFlag = false

				r = 0
				for {
					for {
						y = mem65ClosingBrace + 1
						if y == mem64EqualSignInRule {
							mem61InputPos = mem60InputMatchPos

							if samConfig.Debug {
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
						if inputState.InputTemp[inputTempIndex+1] != mem57CurrentFlags {
							r = 1
							break
						}
						inputTempIndex++
					}

					if r == 0 {
						a := mem57CurrentFlags
						if a == '@' {
							if !code37055(samState, inputTempIndex+1, 4) {
								a = inputState.InputTemp[samState.X]
								if (a != 'R') && (a != 'T') && (a != 'C') && (a != 'S') {
									r = 1
								}
							} else {
								r = -2
							}
						} else if a == ':' {
							for code37055(samState, inputTempIndex+1, 32) {
								inputTempIndex = samState.X
							}
							r = -2
						} else {
							r = handleCh(samState, a, inputTempIndex+1)
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
						inputTempIndex = samState.X
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

func renderVoicedSample(audioState *AudioState, hi uint16, off uint8, phase1 uint8) uint8 {
	for {
		bit := uint8(8)
		sample := sampleTable[hi+uint16(off)]

		for bit != 0 {
			if (sample & 128) != 0 {
				output(audioState, 3, 26)
			} else {
				output(audioState, 4, 6)
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

func combineGlottalAndFormants(speechData *SpeechData, audioState *AudioState, phase1, phase2, phase3, Y uint8) {
	var tmp uint32

	tmp = uint32(multtable[sinus[phase1]|speechData.Amplitude1[Y]])
	tmp += uint32(multtable[sinus[phase2]|speechData.Amplitude2[Y]])

	if tmp > 255 {
		tmp += 1
	} else {
		tmp += 0
	}

	tmp += uint32(multtable[rectangle[phase3]|speechData.Amplitude3[Y]])
	tmp += 136
	tmp >>= 4 // Scale down to 0..15 range of C64 audio.

	output(audioState, 0, uint8(tmp&0xf))
}

func renderSample(speechData *SpeechData, audioState *AudioState, mem66OpenBrace *uint8, consonantFlag, mem49 uint8) {
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
		pitchl = speechData.Pitches[mem49] >> 4
		*mem66OpenBrace = renderVoicedSample(audioState, hi, *mem66OpenBrace, pitchl^255)
	} else {
		renderUnvoicedSample(audioState, hi, pitchl^255, tab48426[hibyte])
	}
}

func renderUnvoicedSample(audioState *AudioState, hi uint16, off, mem53 uint8) {
	for {
		bit := uint8(8)
		sample := sampleTable[hi+uint16(off)]
		for bit != 0 {
			if (sample & 128) != 0 {
				output(audioState, 2, 5)
			} else {
				output(audioState, 1, mem53)
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

func getBufferLength(audioState *AudioState) int {
	return audioState.BufferPos
}

func handleCh2(samState *SamState, ch byte, mem int, inputTemp []byte) int {
	samState.X = byte(mem)
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

// The input[] buffer contains a string of phonemes and stress markers along
// the lines of:
//
//     DHAX KAET IHZ AH5GLIY. <0x9B>
//
// The byte 0x9B marks the end of the buffer. Some phonemes are 2 bytes
// long, such as "DH" and "AX". Others are 1 byte long, such as "T" and "Z".
// There are also stress markers, such as "5" and ".".
//
// The first character of the phonemes are stored in the table
// signInputTable1[]. The second character of the phonemes are stored in the
// table signInputTable2[]. The stress characters are arranged in low to high
// stress order in stressInputTable[].
//
// The following process is used to parse the input[] buffer:
//
// Repeat until the <0x9B> character is reached:
//
//        First, a search is made for a 2 character match for phonemes that do
//        not end with the '*' (wildcard) character. On a match, the index of
//        the phoneme is added to phonemeIndex[] and the buffer position is
//        advanced 2 bytes.
//
//        If this fails, a search is made for a 1 character match against all
//        phoneme names ending with a '*' (wildcard). If this succeeds, the
//        phoneme is added to phonemeIndex[] and the buffer position is advanced
//        1 byte.
//
//        If this fails, search for a 1 character match in the
//        stressInputTable[]. If this succeeds, the stress value is placed in
//        the last stress[] table at the same index of the last added phoneme,
//        and the buffer position is advanced by 1 byte.
//
//        If this fails, return a 0.
//
// On success:
//
//    1. phonemeIndex[] will contain the index of all the phonemes.
//    2. The last index in phonemeIndex[] will be 255.
//    3. stress[] will contain the stress value for each phoneme

// input[] holds the string of phonemes, each two bytes wide
// signInputTable1[] holds the first character of each phoneme
// signInputTable2[] holds te second character of each phoneme
// phonemeIndex[] holds the indexes of the phonemes after parsing input[]
//
// The parser scans through the input[], finding the names of the phonemes
// by searching signInputTable1[] and signInputTable2[]. On a match, it
// copies the index of the phoneme into the phonemeIndexTable[].
//
// The character <0x9B> marks the end of text in input[]. When it is reached,
// the index 255 is placed at the end of the phonemeIndexTable[], and the
// function returns with a 1 indicating success.

func parser1(phonemeState *PhonemeState, inputState *InputState) bool {
	var sign1 byte
	position := byte(0)
	srcpos := byte(0)

	// Clear the stress table
	for i := range phonemeState.Stress[:256] {
		phonemeState.Stress[i] = 0
	}

	for {
		sign1 = inputState.Input[srcpos]
		if sign1 == 155 { // 155 (\233) is end of line marker
			break
		}

		var match int
		srcpos++
		sign2 := inputState.Input[srcpos]

		if match = fullMatch(sign1, sign2); match != -1 {
			// Matched both characters (no wildcards)
			phonemeState.PhonemeIndex[position] = byte(match)
			position++
			srcpos++ // Skip the second character of the input as we've matched it
		} else if match = wildMatch(sign1); match != -1 {
			// Matched just the first character (with second character matching '*')
			phonemeState.PhonemeIndex[position] = byte(match)
			position++
		} else {
			// Should be a stress character. Search through the stress table backwards.
			match = 8 // End of stress table. FIXME: Don't hardcode.
			for sign1 != stressInputTable[match] && match > 0 {
				match--
			}

			if match == 0 {
				return false // failure
			}

			phonemeState.Stress[position-1] = byte(match) // Set stress for prior phoneme
		}
	}

	phonemeState.PhonemeIndex[position] = END
	return true
}

func samMain(samState *SamState) bool {
	samConfig := &samState.Config
	phonemeState := &samState.Phonemes
	inputState := &samState.Input

	if !parser1(phonemeState, inputState) {
		return false
	}
	if samConfig.Debug {
		printPhonemes(phonemeState.PhonemeIndex, phonemeState.PhonemeLength, phonemeState.Stress)
	}
	parser2(samConfig, phonemeState)
	copyStress(phonemeState)
	setPhonemeLength(phonemeState)
	adjustLengths(phonemeState, samConfig)
	code41240(phonemeState)
	for x := byte(0); phonemeState.PhonemeIndex[x] != 255; x++ {
		if phonemeState.PhonemeIndex[x] > 80 {
			phonemeState.PhonemeIndex[x] = 255
			break
		}
	}
	insertBreath(phonemeState)

	if samConfig.Debug {
		printPhonemes(phonemeState.PhonemeIndex, phonemeState.PhonemeLength, phonemeState.Stress)
	}
	prepareOutput(samState)
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

func createFrames(samState *SamState) {
	speechData := &samState.Speech
	phonemeState := &samState.Phonemes
	samConfig := &samState.Config
	samState.X = byte(0)
	for i := 0; i < 256; i++ {
		// get the phoneme at the index
		phoneme := phonemeState.PhonemeIndexOutput[i]

		// if terminal phoneme, exit the loop
		if phoneme == 255 {
			break
		}

		if phoneme == PHONEME_PERIOD {
			addInflection(speechData, RISING_INFLECTION, samState.X)
		} else if phoneme == PHONEME_QUESTION {
			addInflection(speechData, FALLING_INFLECTION, samState.X)
		}

		// get the stress amount (more stress = higher pitch)
		phase1 := tab47492[phonemeState.StressOutput[i]+1]

		// get number of frames to write
		phase2 := phonemeState.PhonemeLengthOutput[i]

		// copy from the source to the frames list
		for phase2 != 0 {
			speechData.Frequency1[samState.X] = freq1data[phoneme]                       // F1 frequency
			speechData.Frequency2[samState.X] = freq2data[phoneme]                       // F2 frequency
			speechData.Frequency3[samState.X] = freq3data[phoneme]                       // F3 frequency
			speechData.Amplitude1[samState.X] = ampl1data[phoneme]                       // F1 amplitude
			speechData.Amplitude2[samState.X] = ampl2data[phoneme]                       // F2 amplitude
			speechData.Amplitude3[samState.X] = ampl3data[phoneme]                       // F3 amplitude
			speechData.SampledConsonantFlag[samState.X] = sampledConsonantFlags[phoneme] // phoneme data for sampled consonants
			speechData.Pitches[samState.X] = samConfig.Pitch + phase1                    // pitch

			samState.X++
			phase2--
		}
	}
}

func code37055(samState *SamState, npos, mask byte) bool {
	inputState := &samState.Input
	samState.X = npos
	result := tab36376[inputState.InputTemp[samState.X]] & mask
	if result == 0 {
		return false
	} else {
		return true
	}
}

func createTransitions(phonemeState *PhonemeState, speechData *SpeechData) uint8 {
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

		phoneme := phonemeState.PhonemeIndexOutput[pos]
		nextPhoneme := phonemeState.PhonemeIndexOutput[pos+1]

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

		mem49 += phonemeState.PhonemeLengthOutput[pos]

		speedcounter = mem49 + phase2
		phase3 = mem49 - phase1
		transition = phase1 + phase2 // total transition?

		if ((transition - 2) & 128) == 0 {
			table := uint8(169)
			interpolatePitch(speechData, phonemeState, pos, mem49, phase3)
			for table < 175 {
				// tables:
				// 168  pitches[]
				// 169  frequency1
				// 170  frequency2
				// 171  frequency3
				// 172  amplitude1
				// 173  amplitude2
				// 174  amplitude3

				value := int8(read(speechData, table, speedcounter)) - int8(read(speechData, table, phase3))
				interpolate(speechData, transition, table, phase3, value)
				table++
			}
		}
		pos++
	}

	// add the length of this phoneme
	return mem49 + phonemeState.PhonemeLengthOutput[pos]
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

func read(speechData *SpeechData, p, y byte) byte {
	switch p {
	case 168:
		return speechData.Pitches[y]
	case 169:
		return speechData.Frequency1[y]
	case 170:
		return speechData.Frequency2[y]
	case 171:
		return speechData.Frequency3[y]
	case 172:
		return speechData.Amplitude1[y]
	case 173:
		return speechData.Amplitude2[y]
	case 174:
		return speechData.Amplitude3[y]
	default:
		panic("Error reading from tables")
	}
}

func addInflection(speechData *SpeechData, inflection, pos byte) {
	end := pos

	if pos < 30 {
		pos = 0
	} else {
		pos -= 30
	}

	var a byte
	for speechData.Pitches[pos] == 127 {
		pos++
	}
	a = speechData.Pitches[pos]

	for pos != end {
		a += inflection
		speechData.Pitches[pos] = a

		for {
			pos++
			if pos == end {
				break // Exit loop if we've reached the end
			}
			if speechData.Pitches[pos] != 255 {
				break // Exit loop if we've found a non-255 pitch
			}
		}
	}
}

func assignPitchContour(speechData *SpeechData) {
	for i := 0; i < 256; i++ {
		speechData.Pitches[i] -= (speechData.Frequency1[i] >> 1)
	}
}

func changeRule(phonemeState *PhonemeState, samConfig *SamConfig, position, mem60InputMatchPos byte, descr string) {
	if samConfig.Debug {
		fmt.Printf("RULE: %s\n", descr)
	}
	phonemeState.PhonemeIndex[position] = 13 // rule
	insert(phonemeState, position+1, mem60InputMatchPos, 0, phonemeState.Stress[position])
}

func change(phonemeState *PhonemeState, samConfig *SamConfig, pos, val byte, rule string) {
	if samConfig.Debug {
		fmt.Printf("RULE: %s\n", rule)
	}
	phonemeState.PhonemeIndex[pos] = val
}

func code41240(phonemeState *PhonemeState) {
	pos := byte(0)
	for phonemeState.PhonemeIndex[pos] != 255 { // 255 == END
		index := phonemeState.PhonemeIndex[pos]
		if (flags[index] & FLAG_STOPCONS) != 0 {
			if (flags[index] & FLAG_PLOSIVE) != 0 {
				x := pos
				for phonemeState.PhonemeIndex[x+1] == 0 {
					x++ // Skip pause
				}
				a := phonemeState.PhonemeIndex[x+1]
				if a != 255 { // END
					if (flags[a]&8) != 0 || a == 36 || a == 37 {
						pos++
						continue
					}
				}
			}
			insert(phonemeState, pos+1, index+1, phonemeLengthTable[index+1], phonemeState.Stress[pos])
			insert(phonemeState, pos+2, index+2, phonemeLengthTable[index+2], phonemeState.Stress[pos])
			pos += 2
		}
		pos++
	}
}

func copyStress(phonemeState *PhonemeState) {
	pos := byte(0)
	for phonemeState.PhonemeIndex[pos] != 255 { // 255 == END
		if (flags[phonemeState.PhonemeIndex[pos]] & 64) != 0 {
			y := phonemeState.PhonemeIndex[pos+1]
			if y != 255 && (flags[y]&128) != 0 {
				y = phonemeState.Stress[pos+1]
				if y != 0 && (y&128) == 0 {
					phonemeState.Stress[pos] = y + 1
				}
			}
		}
		pos++
	}
}

func describeRule(samConfig *SamConfig, str string) {
	if samConfig.Debug {
		fmt.Printf("RULE: %s\n", str)
	}
}

func adjustLengths(phonemeState *PhonemeState, samConfig *SamConfig) {
	// LENGTHEN VOWELS PRECEDING PUNCTUATION
	{
		X := byte(0)
		var index byte

		for index = phonemeState.PhonemeIndex[X]; index != END; index = phonemeState.PhonemeIndex[X] {
			var loopIndex byte

			// not punctuation?
			if (flags[index] & FLAG_PUNCT) == 0 {
				X = X + 1
				continue
			}

			loopIndex = X

			// Back up to the first vowel
			for X > 0 {
				X = X - 1 // Decrement X
				if (flags[phonemeState.PhonemeIndex[X]] & FLAG_VOWEL) != 0 {
					break // Exit the loop if a vowel is found
				}
			}

			if X == 0 {
				break
			}

			for {
				// test for vowel
				index = phonemeState.PhonemeIndex[X]

				// test for fricative/unvoiced or not voiced
				if (flags[index]&FLAG_FRICATIVE) == 0 || (flags[index]&FLAG_VOICED) != 0 { // nochmal überprüfen
					A := phonemeState.PhonemeLength[X]
					// change phoneme length to (length * 1.5) + 1
					describeRulePre(samConfig, phonemeState, "Lengthen <FRICATIVE> or <VOICED> between <VOWEL> and <PUNCTUATION> by 1.5", X)
					phonemeState.PhonemeLength[X] = (A >> 1) + A + 1
					describeRulePost(samConfig, phonemeState, X)
				}
				X = X + 1
				if X == loopIndex {
					break
				}
			}
			X = X + 1
		} // for
	}

	// Similar to the above routine, but shorten vowels under some circumstances

	// Loop through all phonemes
	loopIndex := byte(0)
	var index byte

	for index = phonemeState.PhonemeIndex[loopIndex]; index != END; index = phonemeState.PhonemeIndex[loopIndex] {
		X := loopIndex

		if (flags[index] & FLAG_VOWEL) != 0 {
			index = phonemeState.PhonemeIndex[loopIndex+1]
			if (flags[index] & FLAG_CONSONANT) == 0 {
				if index == 18 || index == 19 { // 'RX', 'LX'
					index = phonemeState.PhonemeIndex[loopIndex+2]
					if (flags[index] & FLAG_CONSONANT) != 0 {
						describeRulePre(samConfig, phonemeState, "<VOWEL> <RX | LX> <CONSONANT> - decrease length of vowel by 1\n", loopIndex)
						phonemeState.PhonemeLength[loopIndex] = phonemeState.PhonemeLength[loopIndex] - 1
						describeRulePost(samConfig, phonemeState, loopIndex)
					}
				}
			} else { // Got here if not <VOWEL>
				var flag uint16
				if index == END {
					flag = 65
				} else {
					flag = flags[index]
				}

				if (flag & FLAG_VOICED) == 0 { // Unvoiced
					// *, .*, ?*, ,*, -*, DX, S*, SH, F*, TH, /H, /X, CH, P*, T*, K*, KX
					if (flag & FLAG_PLOSIVE) != 0 { // unvoiced plosive
						// RULE: <VOWEL> <UNVOICED PLOSIVE>
						// <VOWEL> <P*, T*, K*, KX>
						describeRulePre(samConfig, phonemeState, "<VOWEL> <UNVOICED PLOSIVE> - decrease vowel by 1/8th", loopIndex)
						phonemeState.PhonemeLength[loopIndex] = phonemeState.PhonemeLength[loopIndex] - (phonemeState.PhonemeLength[loopIndex] >> 3)
						describeRulePost(samConfig, phonemeState, loopIndex)
					}
				} else {
					describeRulePre(samConfig, phonemeState, "<VOWEL> <VOICED CONSONANT> - increase vowel by 1/2 + 1\n", X-1)
					// decrease length
					A := phonemeState.PhonemeLength[loopIndex]
					phonemeState.PhonemeLength[loopIndex] = (A >> 2) + A + 1 // 5/4*A + 1
					describeRulePost(samConfig, phonemeState, loopIndex)
				}
			}
		} else if (flags[index] & FLAG_NASAL) != 0 { // nasal?
			// RULE: <NASAL> <STOP CONSONANT>
			//       Set punctuation length to 6
			//       Set stop consonant length to 5
			X = X + 1
			index = phonemeState.PhonemeIndex[X]
			if index != END && (flags[index]&FLAG_STOPCONS) != 0 {
				describeRule(samConfig, "<NASAL> <STOP CONSONANT> - set nasal = 5, consonant = 6")
				phonemeState.PhonemeLength[X] = 6   // set stop consonant length to 6
				phonemeState.PhonemeLength[X-1] = 5 // set nasal length to 5
			}
		} else if (flags[index] & FLAG_STOPCONS) != 0 { // (voiced) stop consonant?
			// RULE: <VOICED STOP CONSONANT> {optional silence} <STOP CONSONANT>
			//       Shorten both to (length/2 + 1)

			// move past silence
			X = X + 1
			for phonemeState.PhonemeIndex[X] == 0 {
				X = X + 1
			}
			index = phonemeState.PhonemeIndex[X]

			if index != END && (flags[index]&FLAG_STOPCONS) != 0 {
				// FIXME, this looks wrong?
				// RULE: <UNVOICED STOP CONSONANT> {optional silence} <STOP CONSONANT>
				describeRule(samConfig, "<UNVOICED STOP CONSONANT> {optional silence} <STOP CONSONANT> - shorten both to 1/2 + 1")
				phonemeState.PhonemeLength[X] = (phonemeState.PhonemeLength[X] >> 1) + 1
				phonemeState.PhonemeLength[loopIndex] = (phonemeState.PhonemeLength[loopIndex] >> 1) + 1
				X = loopIndex
			}
		} else if (flags[index] & FLAG_LIQUIC) != 0 { // liquic consonant?
			// RULE: <VOICED NON-VOWEL> <DIPTHONG>
			//       Decrease <DIPTHONG> by 2
			index = phonemeState.PhonemeIndex[X-1] // prior phoneme;

			// FIXME: The debug code here breaks the rule.
			// prior phoneme a stop consonant>
			if (flags[index] & FLAG_STOPCONS) != 0 {
				describeRulePre(samConfig, phonemeState, "<LIQUID CONSONANT> <DIPTHONG> - decrease by 2", X)
			}

			phonemeState.PhonemeLength[X] = phonemeState.PhonemeLength[X] - 2 // 20ms
			describeRulePost(samConfig, phonemeState, X)
		}

		loopIndex = loopIndex + 1
	}
}

func enableSingmode(samConfig *SamConfig) {
	samConfig.SingMode = true
}

func insertBreath(phonemeState *PhonemeState) {
	mem54 := byte(255)
	len := byte(0)
	pos := byte(0)

	for {
		index := phonemeState.PhonemeIndex[pos]
		if index == 255 { // END
			break
		}

		len += phonemeState.PhonemeLength[pos]
		if len < 232 {
			if index == BREAK {
				// Do nothing
			} else if (flags[index] & FLAG_PUNCT) == 0 {
				if index == 0 {
					mem54 = pos
				}
			} else {
				len = 0
				insert(phonemeState, pos+1, BREAK, 0, 0)
			}
		} else {
			pos = mem54
			phonemeState.PhonemeIndex[pos] = 31 // 'Q*' glottal stop
			phonemeState.PhonemeLength[pos] = 4
			phonemeState.Stress[pos] = 0

			len = 0
			insert(phonemeState, pos+1, BREAK, 0, 0)
		}
		pos++
	}
}

func insert(phonemeState *PhonemeState, position, mem60InputMatchPos, mem59, mem58Variant byte) {
	for i := 253; i >= int(position); i-- {
		phonemeState.PhonemeIndex[i+1] = phonemeState.PhonemeIndex[i]
		phonemeState.PhonemeLength[i+1] = phonemeState.PhonemeLength[i]
		phonemeState.Stress[i+1] = phonemeState.Stress[i]
	}

	phonemeState.PhonemeIndex[position] = mem60InputMatchPos
	phonemeState.PhonemeLength[position] = mem59
	phonemeState.Stress[position] = mem58Variant
}

func interpolate(speechData *SpeechData, width, table, frame byte, mem53 int8) {
	sign := mem53 < 0
	remainder := byte(abs(int(mem53))) % width
	div := uint8(int(mem53) / int(width))
	error := byte(0)
	pos := width
	val := read(speechData, table, frame) + div

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
		write(speechData, table, frame, val) // Write updated value back to next frame.
		val += div
		pos--
	}
}

func trans(a, b byte) byte {
	result := byte(((uint16(a) * uint16(b)) >> 8) << 1)
	return result
}

func interpolatePitch(speechData *SpeechData, phonemeState *PhonemeState, pos, mem49, phase3 byte) {
	// half the width of the current and next phoneme
	curWidth := phonemeState.PhonemeLengthOutput[pos] / 2
	nextWidth := phonemeState.PhonemeLengthOutput[pos+1] / 2

	// sum the values
	width := curWidth + nextWidth
	pitch := int8(speechData.Pitches[nextWidth+mem49]) - int8(speechData.Pitches[mem49-curWidth])
	interpolate(speechData, width, 168, phase3, pitch)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func prepareOutput(samState *SamState) {

	phonemeState := &samState.Phonemes
	srcpos := byte(0)
	destpos := byte(0)

	for {
		a := phonemeState.PhonemeIndex[srcpos]
		phonemeState.PhonemeIndexOutput[destpos] = a
		switch a {
		case 255:
			render(samState)
			return
		case 254:
			phonemeState.PhonemeIndexOutput[destpos] = 255
			render(samState)
			destpos = 0
		case 0:
			// Do nothing
		default:
			phonemeState.PhonemeLengthOutput[destpos] = phonemeState.PhonemeLength[srcpos]
			phonemeState.StressOutput[destpos] = phonemeState.Stress[srcpos]
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

func parser2(samConfig *SamConfig, phonemeState *PhonemeState) {
	pos := byte(0) // mem66_openBrace
	var p byte

	if samConfig.Debug {
		fmt.Println("Parser2")
	}

	for p = phonemeState.PhonemeIndex[pos]; p != END; p = phonemeState.PhonemeIndex[pos] {
		var pf uint16
		var prior byte

		if samConfig.Debug {
			fmt.Printf("%d: %c%c\n", pos, signInputTable1[p], signInputTable2[p])
		}

		if p == 0 { // Is phoneme pause?
			pos = pos + 1
			continue
		}

		pf = flags[p]
		prior = phonemeState.PhonemeIndex[pos-1]

		if (pf & FLAG_DIPTHONG) != 0 {
			ruleDipthong(phonemeState, samConfig, p, pf, pos)
		} else if p == 78 {
			changeRule(phonemeState, samConfig, pos, 24, "UL -> AX L") // Example: MEDDLE
		} else if p == 79 {
			changeRule(phonemeState, samConfig, pos, 27, "UM -> AX M") // Example: ASTRONOMY
		} else if p == 80 {
			changeRule(phonemeState, samConfig, pos, 28, "UN -> AX N") // Example: FUNCTION
		} else if (pf&FLAG_VOWEL) != 0 && phonemeState.Stress[pos] != 0 {
			// RULE:
			//       <STRESSED VOWEL> <SILENCE> <STRESSED VOWEL> -> <STRESSED VOWEL>
			//       <SILENCE> Q <VOWEL>
			// EXAMPLE: AWAY EIGHT
			if phonemeState.PhonemeIndex[pos+1] == 0 { // If following phoneme is a pause, get next
				p = phonemeState.PhonemeIndex[pos+2]
				if p != END && (flags[p]&FLAG_VOWEL) != 0 && phonemeState.Stress[pos+2] != 0 {
					describeRule(samConfig, "Insert glottal stop between two stressed vowels with space between them")
					insert(phonemeState, pos+2, 31, 0, 0) // 31 = 'Q'
				}
			}
		} else if p == pR { // RULES FOR PHONEMES BEFORE R
			if prior == pT {
				change(phonemeState, samConfig, pos-1, 42, "T R -> CH R") // Example: TRACK
			} else if prior == pD {
				change(phonemeState, samConfig, pos-1, 44, "D R -> J R") // Example: DRY
			} else if (flags[prior] & FLAG_VOWEL) != 0 {
				change(phonemeState, samConfig, pos, 18, "<VOWEL> R -> <VOWEL> RX") // Example: ART
			}
		} else if p == 24 && (flags[prior]&FLAG_VOWEL) != 0 {
			change(phonemeState, samConfig, pos, 19, "<VOWEL> L -> <VOWEL> LX") // Example: ALL
		} else if prior == 60 && p == 32 { // 'G' 'S'
			// Can't get to fire -
			//       1. The G -> GX rule intervenes
			//       2. Reciter already replaces GS -> GZ
			change(phonemeState, samConfig, pos, 38, "G S -> G Z")
		} else if p == 60 {
			ruleG(samConfig, phonemeState, pos)
		} else {
			if p == 72 { // 'K'
				// K <VOWEL OR DIPTHONG NOT ENDING WITH IY> -> KX <VOWEL OR DIPTHONG NOT
				// ENDING WITH IY> Example: COW
				Y := phonemeState.PhonemeIndex[pos+1]
				// If at end, replace current phoneme with KX
				if (flags[Y]&FLAG_DIP_YX) == 0 ||
					Y == END { // VOWELS AND DIPTHONGS ENDING WITH IY SOUND flag set?
					change(phonemeState, samConfig, pos, 75, "K <VOWEL OR DIPTHONG NOT ENDING WITH IY> -> KX <VOWEL OR DIPTHONG NOT ENDING WITH IY>")
					p = 75
					pf = flags[p]
				}
			}

			// Replace with softer version?
			if (flags[p]&FLAG_PLOSIVE) != 0 && prior == 32 { // 'S'
				// RULE:
				//      S P -> S B
				//      S T -> S D
				//      S K -> S G
				//      S KX -> S GX
				// Examples: SPY, STY, SKY, SCOWL

				if samConfig.Debug {
					fmt.Printf("RULE: S* %c%c -> S* %c%c\n", signInputTable1[p],
						signInputTable2[p], signInputTable1[p-12],
						signInputTable2[p-12])
				}
				phonemeState.PhonemeIndex[pos] = p - 12
			} else if (pf & FLAG_PLOSIVE) == 0 {
				p = phonemeState.PhonemeIndex[pos]
				if p == 53 {
					ruleAlveolarUw(phonemeState, samConfig, pos) // Example: NEW, DEW, SUE, ZOO, THOO, TOO
				} else if p == 42 {
					ruleCh(phonemeState, samConfig, pos) // Example: CHEW
				} else if p == 44 {
					ruleJ(phonemeState, samConfig, pos) // Example: JAY
				}
			}

			if p == 69 || p == 57 { // 'T', 'D'
				// RULE: Soften T following vowel
				// NOTE: This rule fails for cases such as "ODD"
				//       <UNSTRESSED VOWEL> T <PAUSE> -> <UNSTRESSED VOWEL> DX <PAUSE>
				//       <UNSTRESSED VOWEL> D <PAUSE>  -> <UNSTRESSED VOWEL> DX <PAUSE>
				// Example: PARTY, TARDY
				if (flags[phonemeState.PhonemeIndex[pos-1]] & FLAG_VOWEL) != 0 {
					p = phonemeState.PhonemeIndex[pos+1]
					if p == 0 {
						p = phonemeState.PhonemeIndex[pos+2]
					}
					if (flags[p]&FLAG_VOWEL) != 0 && phonemeState.Stress[pos+1] == 0 {
						change(phonemeState, samConfig, pos, 30, "Soften T or D following vowel or ER and preceding a pause -> DX")
					}
				}
			}
		}
		pos = pos + 1
	} // for
}

func processFrames(speechData *SpeechData, samConfig *SamConfig, audioState *AudioState, mem48 byte) {
	speedcounter := byte(72)
	phase1 := byte(0)
	phase2 := byte(0)
	phase3 := byte(0)
	mem66OpenBrace := byte(0)

	y := byte(0)

	glottalPulse := speechData.Pitches[0]
	mem38 := glottalPulse - (glottalPulse >> 2) // mem44 * 0.75

	for mem48 != 0 {
		flags := speechData.SampledConsonantFlag[y]

		// unvoiced sampled phoneme?
		if (flags & 248) != 0 {
			renderSample(speechData, audioState, &mem66OpenBrace, flags, y)
			// skip ahead two in the phoneme buffer
			y += 2
			mem48 -= 2
			speedcounter = samConfig.Speed
		} else {
			combineGlottalAndFormants(speechData, audioState, phase1, phase2, phase3, y)

			speedcounter--
			if speedcounter == 0 {
				y++ // go to next amplitude
				// decrement the frame count
				mem48--
				if mem48 == 0 {
					return
				}
				speedcounter = samConfig.Speed
			}

			glottalPulse--

			if glottalPulse != 0 {
				// not finished with a glottal pulse
				mem38--
				// within the first 75% of the glottal pulse?
				// is the count non-zero and the sampled flag is zero?
				if mem38 != 0 || flags == 0 {
					// reset the phase of the formants to match the pulse
					phase1 += speechData.Frequency1[y]
					phase2 += speechData.Frequency2[y]
					phase3 += speechData.Frequency3[y]
					continue
				}

				// voiced sampled phonemes interleave the sample with the
				// glottal pulse. The sample flag is non-zero, so render
				// the sample for the phoneme.
				renderSample(speechData, audioState, &mem66OpenBrace, flags, y)
			}
		}

		glottalPulse = speechData.Pitches[y]
		mem38 = glottalPulse - (glottalPulse >> 2) // mem44 * 0.75

		// reset the formant wave generators to keep them in
		// sync with the glottal pulse
		phase1 = 0
		phase2 = 0
		phase3 = 0
	}
}

func render(samState *SamState) {

	phonemeState := &samState.Phonemes
	samConfig := &samState.Config
	speechData := &samState.Speech
	audioState := &samState.Audio

	if phonemeState.PhonemeIndexOutput[0] == 255 {
		return // exit if no data
	}

	createFrames(samState)
	t := createTransitions(phonemeState, speechData)

	if !samConfig.SingMode {
		assignPitchContour(speechData)
	}
	rescaleAmplitude(speechData)

	if samConfig.Debug {
		printOutput(speechData.SampledConsonantFlag, speechData.Frequency1, speechData.Frequency2, speechData.Frequency3, speechData.Amplitude1, speechData.Amplitude2, speechData.Amplitude3, speechData.Pitches)
	}

	processFrames(speechData, samConfig, audioState, t)
}

// RESCALE AMPLITUDE
//
// Rescale volume from a linear scale to decibels.
func rescaleAmplitude(speechData *SpeechData) {
	for i := 255; i >= 0; i-- {
		speechData.Amplitude1[i] = amplitudeRescale[speechData.Amplitude1[i]]
		speechData.Amplitude2[i] = amplitudeRescale[speechData.Amplitude2[i]]
		speechData.Amplitude3[i] = amplitudeRescale[speechData.Amplitude3[i]]
	}
}

func output(audioState *AudioState, index int, A byte) {
	audioState.BufferPos += timetable[audioState.OldTimeTableIndex][index]
	audioState.OldTimeTableIndex = index
	// write a little bit in advance
	for k := 0; k < 5; k++ {
		audioState.Buffer[audioState.BufferPos/50+k] = (A & 15) * 16
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

func ruleAlveolarUw(phonemeState *PhonemeState, samConfig *SamConfig, x byte) {
	// ALVEOLAR flag set?
	if (flags[phonemeState.PhonemeIndex[x-1]] & FLAG_ALVEOLAR) != 0 {
		describeRule(samConfig, "<ALVEOLAR> UW -> <ALVEOLAR> UX")
		phonemeState.PhonemeIndex[x] = 16
	}
}

func ruleCh(phonemeState *PhonemeState, samConfig *SamConfig, x byte) {
	describeRule(samConfig, "CH -> CH CH+1")
	insert(phonemeState, x+1, 43, 0, phonemeState.Stress[x])
}

func ruleDipthong(phonemeState *PhonemeState, samConfig *SamConfig, p byte, pf uint16, pos byte) {
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
		describeRule(samConfig, "insert WX following dipthong NOT ending in IY sound")
	} else if a == 21 {
		describeRule(samConfig, "insert YX following dipthong ending in IY sound")
	}
	insert(phonemeState, pos+1, a, 0, phonemeState.Stress[pos])

	if p == 53 {
		ruleAlveolarUw(phonemeState, samConfig, pos) // Example: NEW, DEW, SUE, ZOO, THOO, TOO
	} else if p == 42 {
		ruleCh(phonemeState, samConfig, pos) // Example: CHEW
	} else if p == 44 {
		ruleJ(phonemeState, samConfig, pos) // Example: JAY
	}
}

func ruleG(samConfig *SamConfig, phonemeState *PhonemeState, pos byte) {
	// G <VOWEL OR DIPTHONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPTHONG NOT ENDING WITH IY>
	// Example: GO

	index := phonemeState.PhonemeIndex[pos+1]

	// If dipthong ending with YX, move continue processing next phoneme
	if index != 255 && ((flags[index] & FLAG_DIP_YX) == 0) {
		// replace G with GX and continue processing next phoneme
		describeRule(samConfig, "G <VOWEL OR DIPTHONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPTHONG NOT ENDING WITH IY>")
		phonemeState.PhonemeIndex[pos] = 63 // 'GX'
	}
}

func ruleJ(phonemeState *PhonemeState, samConfig *SamConfig, x byte) {
	describeRule(samConfig, "J -> J J+1")
	insert(phonemeState, x+1, 45, 0, phonemeState.Stress[x])
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

func setPhonemeLength(phonemeState *PhonemeState) {
	position := byte(0)
	for phonemeState.PhonemeIndex[position] != 255 {
		a := phonemeState.Stress[position]
		if a == 0 || (a&128) != 0 {
			phonemeState.PhonemeLength[position] = phonemeLengthTable[phonemeState.PhonemeIndex[position]]
		} else {
			phonemeState.PhonemeLength[position] = phonemeStressedLengthTable[phonemeState.PhonemeIndex[position]]
		}
		position++
	}
}

func setPitch(samConfig *SamConfig, pitchSource byte) {
	samConfig.Pitch = pitchSource
}

func setSpeed(samConfig *SamConfig, speedSource byte) {
	samConfig.Speed = speedSource
}

func setThroat(samConfig *SamConfig, _throat byte) {
	samConfig.Throat = _throat
}

func setMouth(samConfig *SamConfig, _mouth byte) {
	samConfig.Mouth = _mouth
}

func write(speechData *SpeechData, p, y, value byte) {
	switch p {
	case 168:
		speechData.Pitches[y] = value
	case 169:
		speechData.Frequency1[y] = value
	case 170:
		speechData.Frequency2[y] = value
	case 171:
		speechData.Frequency3[y] = value
	case 172:
		speechData.Amplitude1[y] = value
	case 173:
		speechData.Amplitude2[y] = value
	case 174:
		speechData.Amplitude3[y] = value
	default:
		panic("Error writing to tables")
	}
}

func writeWav(filename string, buffer []byte, bufferLength int) error {
	file, err := os.OpenFile(filename, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)

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

	channels := uint16(SampleChannels)
	if err := binary.Write(file, binary.LittleEndian, channels); err != nil {
		return fmt.Errorf("failed to write number of channels: %v", err)
	}

	sampleRate := uint32(SampleRate)
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

	buffer = buffer[:bufferLength] // Dirty workaround. Truncate the 10 second buffer
	if _, err := file.Write(buffer); err != nil {
		return fmt.Errorf("failed to write audio data: %v", err)
	}
	return nil
}

func main() {
	var samState SamState
	audioState := &samState.Audio
	inputState := &samState.Input
	samConfig := &samState.Config
	initThings(&samState)

	var phonetic bool
	var wavFilename string

	inputState.Input = make([]byte, 256)

	if len(os.Args) <= 1 {
		printUsage()
		os.Exit(1)
	}

	i := 1
	for i < len(os.Args) {
		if os.Args[i][0] != '-' {
			stringConcatenateSafe(inputState.Input, 256, strings.ToUpper(os.Args[i]+" "))
		} else {
			switch os.Args[i][1:] {
			case "wav":
				if i+1 < len(os.Args) {
					wavFilename = os.Args[i+1]
					i++
				}
			case "sing":
				enableSingmode(samConfig)
			case "phonetic":
				phonetic = true
			case "debug":
				samConfig.Debug = true
			case "pitch":
				if i+1 < len(os.Args) {
					pitch, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setPitch(samConfig, byte(min(pitch, 255)))
					}
					i++
				}
			case "speed":
				if i+1 < len(os.Args) {
					speed, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setSpeed(samConfig, byte(min(speed, 255)))
					}
					i++
				}
			case "mouth":
				if i+1 < len(os.Args) {
					mouth, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setMouth(samConfig, byte(min(mouth, 255)))
					}
					i++
				}
			case "throat":
				if i+1 < len(os.Args) {
					throat, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						setThroat(samConfig, byte(min(throat, 255)))
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

	if samConfig.Debug {
		if phonetic {
			fmt.Printf("phonetic input: %s\n", nullTerminatedBytesToString(inputState.Input))
		} else {
			fmt.Printf("text input: %s\n", nullTerminatedBytesToString(inputState.Input))
		}
	}

	if !phonetic {
		stringConcatenateSafe(inputState.Input, 256, "[")
		if !textToPhonemes(&samState, inputState.Input) {
			os.Exit(1)
		}
		if samConfig.Debug {
			fmt.Printf("phonetic input: %s\n", nullTerminatedBytesToString(inputState.Input))
		}
	} else {
		stringConcatenateSafe(inputState.Input, 256, "\x9b")
	}

	setInput(inputState.Input)
	if !samMain(&samState) {
		printUsage()
		os.Exit(1)
	}

	var err error

	if wavFilename != "" {
		err = writeWav(wavFilename, getBuffer(audioState), getBufferLength(audioState)/50)
	} else {
		err = playAudio(audioState, getBuffer(audioState), getBufferLength(audioState)/50)

		if err != nil {
			log.Fatalf("Failed to output audio: %v", err)
		}
	}

	if err != nil {
		log.Fatalf("Failed to open log file: %v", err)
	}
}

func nullTerminatedBytesToString(b []byte) string {
	for i, v := range b {
		if v == 0 {
			return string(b[:i])
		}
	}
	return string(b)
}

func getBuffer(audioState *AudioState) []byte {
	return audioState.Buffer
}

// Concatenate a source string to a destination string/buffer while preventing
// buffer overflows by ensuring the total length does not exceed a specified
// buffer size.
func stringConcatenateSafe(dest []byte, size int, str string) {
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

func initThings(samState *SamState) {
	phonemeState := &samState.Phonemes
	speechData := &samState.Speech
	audioState := &samState.Audio
	samConfig := &samState.Config

	// Initialize various arrays to default values (should be improved!)
	phonemeState.PhonemeIndex = make([]byte, 256)
	phonemeState.PhonemeLength = make([]byte, 256)
	phonemeState.Stress = make([]byte, 256)
	phonemeState.PhonemeIndexOutput = make([]byte, 60)
	phonemeState.StressOutput = make([]byte, 60)
	phonemeState.PhonemeLengthOutput = make([]byte, 60)
	speechData.Frequency1 = make([]byte, 256)
	speechData.Frequency2 = make([]byte, 256)
	speechData.Frequency3 = make([]byte, 256)
	speechData.Amplitude1 = make([]byte, 256)
	speechData.Amplitude2 = make([]byte, 256)
	speechData.Amplitude3 = make([]byte, 256)
	speechData.SampledConsonantFlag = make([]byte, 256)
	speechData.Pitches = make([]byte, 256)

	samConfig.Speed = 72
	samConfig.Pitch = 64
	samConfig.Mouth = 128
	samConfig.Throat = 128
	samConfig.SingMode = false
	samConfig.Debug = false

	audioState.BufferPos = 0
	audioState.OldTimeTableIndex = 0
	audioState.Buffer = make([]byte, SampleRate*10)

	setMouthThroat(samConfig.Mouth, samConfig.Throat)

	// TODO: check for free memory, 10 seconds of output should be more than enough

	if err := initAudio(audioState); err != nil {
		log.Fatalf("Failed to initialize audio: %v", err)
	}

	for i := 0; i < 256; i++ {
		phonemeState.Stress[i] = 0
		phonemeState.PhonemeLength[i] = 0
	}

	for i := 0; i < 60; i++ {
		phonemeState.PhonemeIndexOutput[i] = 0
		phonemeState.StressOutput[i] = 0
		phonemeState.PhonemeLengthOutput[i] = 0
	}
	phonemeState.PhonemeIndex[255] = 255 // to prevent buffer overflow

	// ML : changed from 32 to 255 to stop freezing with long inputs

}

func describeRulePost(samConfig *SamConfig, phonemeState *PhonemeState, x byte) {
	if samConfig.Debug {
		fmt.Println("POST")
		fmt.Printf("phoneme %d (%c%c) length %d\n", x, signInputTable1[phonemeState.PhonemeIndex[x]],
			signInputTable2[phonemeState.PhonemeIndex[x]], phonemeState.PhonemeLength[x])
	}
}

func describeRulePre(samConfig *SamConfig, phonemeState *PhonemeState, descr string, x byte) {
	describeRule(samConfig, descr)
	if samConfig.Debug {
		fmt.Println("PRE")
		fmt.Printf(
			"phoneme %d (%c%c) length %d\n",
			x,
			signInputTable1[phonemeState.PhonemeIndex[x]],
			signInputTable2[phonemeState.PhonemeIndex[x]],
			phonemeState.PhonemeLength[x],
		)
	}
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

func handleCh(samState *SamState, ch, mem byte) int {
	inputState := &samState.Input
	samState.X = mem
	tmp := tab36376[inputState.InputTemp[mem]]
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
			if inputState.InputTemp[mem] != 72 {
				return 1
			}
			mem++
		}
	} else if ch == '^' {
		if (tmp & 32) == 0 {
			return 1
		}
	} else if ch == '+' {
		ch = inputState.InputTemp[mem]
		if ch != 69 && ch != 73 && ch != 89 {
			return 1
		}
	} else {
		return -1
	}
	return 0
}
