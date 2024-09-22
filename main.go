package main

import (
	"bytes"
	"encoding/binary"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/pdxiv/samgo/synthesizer"
)

type IntOrFloat interface {
	~int | ~float64
}

type sequencerEvent struct {
	Duration float64
	Notes    []string
	Phonemes string
}

func main() {
	var samState synthesizer.SamState
	audioState := &samState.Audio
	inputState := &samState.Input
	samConfig := &samState.Config

	var phonetic bool
	var sequencer bool
	var wavFilename string

	inputState.Input = make([]byte, 256)

	// Define flags
	flag.BoolVar(&samConfig.Debug, "debug", false, "print additional debug messages")
	flag.Float64Var(&samConfig.Frequency, "frequency", 0, "set frequency value")
	flag.BoolVar(&samConfig.Hifi, "hifi", false, "enable hifi")
	flag.Float64Var(&samConfig.Length, "length", 0, "set length")
	flag.UintVar(&samConfig.Mouth, "mouth", 128, "set mouth value")
	flag.StringVar(&samConfig.Note, "note", "", "set note")
	flag.BoolVar(&phonetic, "phonetic", false, "enter phonetic mode")
	flag.Float64Var(&samConfig.Pitch, "pitch", 64, "set pitch value")
	flag.BoolVar(&samConfig.Robot, "robot", false, "enable robot")
	flag.BoolVar(&samConfig.SingMode, "sing", false, "enable sing mode")
	flag.Float64Var(&samConfig.Speed, "speed", 72, "set speed value")
	flag.UintVar(&samConfig.Throat, "throat", 128, "set throat value")
	flag.StringVar(&wavFilename, "wav", "", "output to wav instead of libsdl")
	flag.BoolVar(&sequencer, "sequencer", false, "enter sequencer mode")

	// Parse flags
	flag.Parse()

	// Check if there are no positional arguments after flags
	if flag.NArg() == 0 {
		printUsage()
		os.Exit(1)
	}

	// Concatenate positional arguments into inputState.Input
	for _, arg := range flag.Args() {
		stringConcatenateSafe(inputState.Input, 256, strings.ToUpper(arg+" "))
		fmt.Printf("DEBUG: inputState.Input: \"%s\"\n", inputState.Input)
	}

	// Handle the -note flag to set Pitch
	if samConfig.Note != "" {
		samConfig.Robot = true
		pitchValue, err := synthesizer.NoteToPitch(samConfig.Note)
		if err != nil {
			log.Fatalf("Invalid note '%s': %v", samConfig.Note, err)
		}
		if !samConfig.Robot {
			pitchValue = math.Round(pitchValue)
		}
		samConfig.Pitch = min(pitchValue, 255)
	}

	if samConfig.Frequency > 0 {
		samConfig.Robot = true
		frequencyValue := synthesizer.FrequencyToPitch(samConfig.Frequency)
		samConfig.Pitch = min(frequencyValue, 255)
	}

	if samConfig.Debug {
		if phonetic {
			fmt.Printf("phonetic input: %s\n", nullTerminatedBytesToString(inputState.Input))
		} else {
			fmt.Printf("text input: %s\n", nullTerminatedBytesToString(inputState.Input))
		}
	}

	if phonetic {
		stringConcatenateSafe(inputState.Input, 256, "\x9b")
	} else {
		stringConcatenateSafe(inputState.Input, 256, "[")
		if !synthesizer.TextToPhonemes(&samState, inputState.Input) {
			os.Exit(1)
		}
		if samConfig.Debug {
			fmt.Printf("phonetic input: %s\n", nullTerminatedBytesToString(inputState.Input))
		}

	}

	synthesizer.InitThings(&samState)

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
	fmt.Println("  -phonetic         Enters phonetic mode (see below)")
	fmt.Println("  -pitch number     Set pitch value (default=64)")
	fmt.Println("  -speed number     Set speed value (default=72)")
	fmt.Println("  -throat number    Set throat value (default=128)")
	fmt.Println("  -mouth number     Set mouth value (default=128)")
	fmt.Println("  -wav filename     Output to wav instead of libsdl")
	fmt.Println("  -sing             Special treatment of pitch")
	fmt.Println("  -frequency number Set frequency value")
	fmt.Println("  -hifi             Enable hifi mode")
	fmt.Println("  -length number    Set length value")
	fmt.Println("  -note string      Set note (e.g., C4, D#5)")
	fmt.Println("  -robot            Enable robot mode")
	fmt.Println("  -sequence         Enters sequencer mode.")
	fmt.Println("  -debug            Print additional debug messages")
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

func getSequencerEvents(input string) []sequencerEvent {
	const secondsInMinute = 60

	tempo := 120.0 // Set default tempo to 120 BPM
	var notes []string
	var events []sequencerEvent

	upperCaseInput := strings.ToUpper(input)

	allCommandsPattern := regexp.MustCompile(`(TEMPO\s*\d+(?:\.\d+)?|[A-G][#-]?\d+(?: *, *[A-G][#-]?\d+)*|\([0-9A-Z ./]+\) *\ *{ *\d+(?:\.\d+)? *\})`)

	tempoPattern := regexp.MustCompile(`TEMPO\s*(\d+(?:\.\d+)?)`)
	notesPattern := regexp.MustCompile(`([A-G][#-]?\d+(?: *, *[A-G][#-]?\d+)*)`)
	notePattern := regexp.MustCompile(`([A-G][#-]?\d+)`)
	phonemePattern := regexp.MustCompile(`\(([0-9A-Z ./]+)\) *\ *{ *(\d+(?:\.\d+)?) *\}`)

	commandMatches := allCommandsPattern.FindAllString(upperCaseInput, -1)
	for _, command := range commandMatches {
		switch {
		case tempoPattern.MatchString(command):
			match := tempoPattern.FindStringSubmatch(command)
			tempoString := match[1]

			f, err := strconv.ParseFloat(tempoString, 64)
			if err != nil {
				fmt.Println("Error:", err)
			} else {
				tempo = f
			}

		case notesPattern.MatchString(command):
			notesString := notesPattern.FindString(command)
			individualNotes := notePattern.FindAllString(notesString, -1)
			notes = []string{}
			notes = append(notes, individualNotes...)

		case phonemePattern.MatchString(command):
			match := phonemePattern.FindStringSubmatch(command)
			phonemes := match[1]
			durationString := match[2]

			var duration float64
			f, err := strconv.ParseFloat(durationString, 64)
			if err != nil {
				fmt.Println("Error:", err)
			} else {
				duration = f
			}

			currentEvent := sequencerEvent{
				Duration: secondsInMinute * duration / tempo,
				Notes:    notes,
				Phonemes: phonemes,
			}
			events = append(events, currentEvent)

		default:
			fmt.Printf("Error: Unknown command: %s\n", command)
		}
	}
	return events
}
