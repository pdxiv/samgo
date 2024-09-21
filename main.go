package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/pdxiv/samgo/synthesizer"
)

type IntOrFloat interface {
	~int | ~float64
}

func main() {
	var samState synthesizer.SamState
	audioState := &samState.Audio
	inputState := &samState.Input
	samConfig := &samState.Config
	synthesizer.InitThings(&samState)

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
				samConfig.SingMode = true
			case "phonetic":
				phonetic = true
			case "debug":
				samConfig.Debug = true
			case "robot":
				samConfig.Robot = true
			case "hifi":
				samConfig.Hifi = true
			case "pitch":
				if i+1 < len(os.Args) {
					pitch, err := strconv.ParseFloat(os.Args[i+1], 64)
					if err == nil {
						if !samConfig.Robot {
							pitch = math.Round(pitch)
						}
						samConfig.Pitch = min(pitch, 255)
					}
					i++
				}
			case "frequency":
				if i+1 < len(os.Args) {
					frequency, err := strconv.ParseFloat(os.Args[i+1], 64)
					if err == nil {
						pitchValue := synthesizer.FrequencyToPitch(frequency)
						if !samConfig.Robot {
							pitchValue = math.Round(pitchValue)
						}
						samConfig.Pitch = min(pitchValue, 255)
					}
					i++
				}
			case "note":
				if i+1 < len(os.Args) {
					note := os.Args[i+1]
					pitchValue, err := synthesizer.NoteToPitch(note)
					if err == nil {
						if !samConfig.Robot {
							pitchValue = math.Round(pitchValue)
						}
						samConfig.Pitch = min(pitchValue, 255)
					}
					i++
				}
			case "speed":
				if i+1 < len(os.Args) {
					speed, err := strconv.ParseFloat(os.Args[i+1], 64)
					if err == nil {
						samConfig.Speed = min(speed, 255)
					}
					i++
				}
			case "mouth":
				if i+1 < len(os.Args) {
					mouth, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						samConfig.Mouth = byte(min(mouth, 255))
					}
					i++
				}
			case "throat":
				if i+1 < len(os.Args) {
					throat, err := strconv.Atoi(os.Args[i+1])
					if err == nil {
						samConfig.Throat = byte(min(throat, 255))
					}
					i++
				}
			case "length":
				if i+1 < len(os.Args) {
					speed, err := strconv.ParseFloat(os.Args[i+1], 64)
					if err == nil {
						samConfig.Length = min(speed, 255)
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
		if !synthesizer.TextToPhonemes(&samState, inputState.Input) {
			os.Exit(1)
		}
		if samConfig.Debug {
			fmt.Printf("phonetic input: %s\n", nullTerminatedBytesToString(inputState.Input))
		}
	} else {
		stringConcatenateSafe(inputState.Input, 256, "\x9b")
	}

	if !synthesizer.SamMain(&samState) {
		printUsage()
		os.Exit(1)
	}

	var err error

	if wavFilename != "" {
		err = writeWav(wavFilename, audioState.Buffer)
	} else {
		err = playAudio(audioState, audioState.Buffer)

		if err != nil {
			log.Fatalf("Failed to output audio: %v", err)
		}
	}

	if err != nil {
		log.Fatalf("Failed to open log file: %v", err)
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

// Generic min function
func min[T IntOrFloat](l, r T) T {
	if l < r {
		return l
	}
	return r
}

func nullTerminatedBytesToString(b []byte) string {
	for i, v := range b {
		if v == 0 {
			return string(b[:i])
		}
	}
	return string(b)
}

func writeWav(filename string, buffer []byte) error {
	bufferLength := len(buffer)
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

	channels := uint16(synthesizer.SampleChannels)
	if err := binary.Write(file, binary.LittleEndian, channels); err != nil {
		return fmt.Errorf("failed to write number of channels: %v", err)
	}

	sampleRate := uint32(synthesizer.SampleRate)
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

	// buffer = buffer[:bufferLength] // Dirty workaround. Truncate the 10 second buffer

	if _, err := file.Write(buffer); err != nil {
		return fmt.Errorf("failed to write audio data: %v", err)
	}
	return nil
}

func playAudio(audioState *synthesizer.AudioState, buffer []byte) error {
	bufferLength := len(buffer)
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
		time.Sleep(100 * time.Millisecond)
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
