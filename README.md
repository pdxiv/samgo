# SAM (Software Automatic Mouth) - Go Port

## What is SAM?

SAM (Software Automatic Mouth) is a vintage text-to-speech engine originally developed for the Commodore 64 in 1982 by Don't Ask Software (now SoftVoice, Inc.). This project is a Go port of SAM based on the C adaptation by [Stefan Macke](https://github.com/s-macke/SAM).

## Features

* Text-to-speech conversion
* Phonetic input mode
* Adjustable pitch, speed, mouth, and throat settings
* Singing mode
* WAV file output
* Debug mode for detailed processing information

## Installation

To install SAM, ensure you have Go installed on your system, then run the following:

```bash
go install github.com/pdxiv/samgo@latest
```

This command will download the source code, compile it, and install the `samgo` binary in your Go bin directory.

### Verifying the Installation

After installation, you may need to ensure the Go bin directory is in your system's PATH. Here's how you can check and set it up if needed:

1. Find your Go bin directory:
   

```bash
   go env GOPATH
   ```

   The Go bin directory is typically `$(go env GOPATH)/bin` .

2. Check if it's in your PATH:
   

```bash
   echo $PATH
   ```

   Look for the Go bin directory in the output.

3. If it's not in your PATH, add it:
   - For Bash (Linux/macOS), add this line to your `~/.bashrc` or `~/.bash_profile` :

```bash
     export PATH=$PATH:$(go env GOPATH)/bin
   ```

   - For Zsh (macOS), add the same line to `~/.zshrc` :

```bash
     export PATH=$PATH:$(go env GOPATH)/bin
   ```

   - For Windows (PowerShell), add this to your PowerShell profile:

```powershell
     $env:Path += ";$(go env GOPATH)\bin"
   ```

4. After modifying your shell configuration, restart your terminal or run `source` on the modified file (e.g.,          `source ~/.bashrc`) to apply the changes.

5. Verify the installation by running:
   

```bash
   samgo
   ```

   If installed correctly, this should display SAM's standard help text.

## Usage

Basic usage:

```
samgo "Hello, world!"
```

Using phonetic input:

```
samgo -phonetic "HEH4LOW WER4LD"
```

Adjusting speech parameters:

```
samgo -pitch 64 -speed 72 -mouth 128 -throat 128 "Hello, world!"
```

Output to WAV file:

```
samgo -wav output.wav "Hello, world!"
```

## Dependencies

This project uses the following external library:

* github.com/ebitengine/oto/v3 for audio output

## Current Status

The Go port of SAM is functional with the following notes:

* All core features from the original SAM have been implemented.
* WAV file output works correctly.
* Tuning of voiced consonants is off, due to excessive quantization.
* Direct audio output is functional but may sound slightly garbled compared to the WAV output.

## Acknowledgments

* Original SAM software by Don't Ask Software (SoftVoice, Inc.)
* C port by Stefan Macke (https://github.com/s-macke/SAM)

## Other things

The text-to-phoneme conversion should use proper lists of strings, instead of big byte arrays. It should look something like this:

```go
var rulesString = []string{
	"]A",
	" (A.)=EH4Y. ",
	"(A) =AH",
	" (ARE) =AAR",
	" (AR)O=AXR",
	"(AR)#=EH4R",
	" ^(AS)#=EY4S",
	"(A)WA=AX",
	"(AW)=AO5",
	" :(ANY)=EH4NIY",
	"(A)^+#=EY5",
	"#:(ALLY)=ULIY",
	" (AL)#=UL",
	"(AGAIN)=AXGEH4N",
	"#:(AG)E=IHJ",
	"(A)^%=EY",
	"(A)^+:#=AE",
	" :(A)^+ =EY4",
	" (ARR)=AXR",
	"(ARR)=AE4R",
	" ^(AR) =AA5R",
	"(AR)=AA5R",
	"(AIR)=EH4R",
	"(AI)=EY4",
	"(AY)=EY5",
	"(AU)=AO4",
	"#:(AL) =UL",
	"#:(ALS) =ULZ",
	"(ALK)=AO4K",
	"(AL)^=AOL",
	" :(ABLE)=EY4BUL",
	"(ABLE)=AXBUL",
	"(A)VO=EY4",
	"(ANG)+=EY4NJ",
	"(ATARI)=AHTAA4RIY",
	"(A)TOM=AE",
	"(A)TTI=AE",
	" (AT) =AET",
	" (A)T=AH",
	"(A)=AE",

	"]B",
	" (B) =BIY4",
	" (BE)^#=BIH",
	"(BEING)=BIY4IHNX",
	" (BOTH) =BOW4TH",
	" (BUS)#=BIH4Z",
	"(BREAK)=BREY5K",
	"(BUIL)=BIH4L",
	"(B)=B",

	"]C",
	" (C) =SIY4",
	" (CH)^=K",
	"^E(CH)=K",
	"(CHA)R#=KEH5",
	"(CH)=CH",
	" S(CI)#=SAY4",
	"(CI)A=SH",
	"(CI)O=SH",
	"(CI)EN=SH",
	"(CITY)=SIHTIY",
	"(C)+=S",
	"(CK)=K",
	"(COMMODORE)=KAA4MAHDOHR",
	"(COM)=KAHM",
	"(CUIT)=KIHT",
	"(CREA)=KRIYEY",
	"(C)=K",

	"]D",
	" (D) =DIY4",
	" (DR.) =DAA4KTER",
	"#:(DED) =DIHD",
	".E(D) =D",
	"#:^E(D) =T",
	" (DE)^#=DIH",
	" (DO) =DUW",
	" (DOES)=DAHZ",
	"(DONE) =DAH5N",
	"(DOING)=DUW4IHNX",
	" (DOW)=DAW",
	"#(DU)A=JUW",
	"#(DU)^#=JAX",
	"(D)=D",

	"]E",
	" (E) =IYIY4",
	"#:(E) =",
	"':^(E) =",
	" :(E) =IY",
	"#(ED) =D",
	"#:(E)D =",
	"(EV)ER=EH4V",
	"(E)^%=IY4",
	"(ERI)#=IY4RIY",
	"(ERI)=EH4RIH",
	"#:(ER)#=ER",
	"(ERROR)=EH4ROHR",
	"(ERASE)=IHREY5S",
	"(ER)#=EHR",
	"(ER)=ER",
	" (EVEN)=IYVEHN",
	"#:(E)W=",
	"@(EW)=UW",
	"(EW)=YUW",
	"(E)O=IY",
	"#:&(ES) =IHZ",
	"#:(E)S =",
	"#:(ELY) =LIY",
	"#:(EMENT)=MEHNT",
	"(EFUL)=FUHL",
	"(EE)=IY4",
	"(EARN)=ER5N",
	" (EAR)^=ER5",
	"(EAD)=EHD",
	"#:(EA) =IYAX",
	"(EA)SU=EH5",
	"(EA)=IY5",
	"(EIGH)=EY4",
	"(EI)=IY4",
	" (EYE)=AY4",
	"(EY)=IY",
	"(EU)=YUW5",
	"(EQUAL)=IY4KWUL",
	"(E)=EH",

	"]F",
	" (F) =EH4F",
	"(FUL)=FUHL",
	"(FRIEND)=FREH5ND",
	"(FATHER)=FAA4DHER",
	"(F)F=",
	"(F)=F",

	"]G",
	" (G) =JIY4",
	"(GIV)=GIH5V",
	" (G)I^=G",
	"(GE)T=GEH5",
	"SU(GGES)=GJEH4S",
	"(GG)=G",
	" B#(G)=G",
	"(G)+=J",
	"(GREAT)=GREY4T",
	"(GON)E=GAO5N",
	"#(GH)=",
	" (GN)=N",
	"(G)=G",

	"]H",
	" (H) =EY4CH",
	" (HAV)=/HAE6V",
	" (HERE)=/HIYR",
	" (HOUR)=AW5ER",
	"(HOW)=/HAW",
	"(H)#=/H",
	"(H)=",

	"]I",
	" (IN)=IHN",
	" (I) =AY4",
	"(I) =AY",
	"(IN)D=AY5N",
	"SEM(I)=IY",
	" ANT(I)=AY",
	"(IER)=IYER",
	"#:R(IED) =IYD",
	"(IED) =AY5D",
	"(IEN)=IYEHN",
	"(IE)T=AY4EH",
	"(I')=AY5",
	" :(I)^%=AY5",
	" :(IE) =AY4",
	"(I)%=IY",
	"(IE)=IY4",
	" (IDEA)=AYDIY5AH",
	"(I)^+:#=IH",
	"(IR)#=AYR",
	"(IZ)%=AYZ",
	"(IS)%=AYZ",
	"I^(I)^#=IH",
	"+^(I)^+=AY",
	"#:^(I)^+=IH",
	"(I)^+=AY",
	"(IR)=ER",
	"(IGH)=AY4",
	"(ILD)=AY5LD",
	" (IGN)=IHGN",
	"(IGN) =AY4N",
	"(IGN)^=AY4N",
	"(IGN)%=AY4N",
	"(ICRO)=AY4KROH",
	"(IQUE)=IY4K",
	"(I)=IH",

	"]J",
	" (J) =JEY4",
	"(J)=J",

	"]K",
	" (K) =KEY4",
	" (K)N=",
	"(K)=K",

	"]L",
	" (L) =EH4L",
	"(LO)C#=LOW",
	"L(L)=",
	"#:^(L)%=UL",
	"(LEAD)=LIYD",
	" (LAUGH)=LAE4F",
	"(L)=L",

	"]M",
	" (M) =EH4M",
	" (MR.) =MIH4STER",
	" (MS.)=MIH5Z",
	" (MRS.) =MIH4SIXZ",
	"(MOV)=MUW4V",
	"(MACHIN)=MAHSHIY5N",
	"M(M)=",
	"(M)=M",

	"]N",
	" (N) =EH4N",
	"E(NG)+=NJ",
	"(NG)R=NXG",
	"(NG)#=NXG",
	"(NGL)%=NXGUL",
	"(NG)=NX",
	"(NK)=NXK",
	" (NOW) =NAW4",
	"N(N)=",
	"(NON)E=NAH4N",
	"(N)=N",

	"]O",
	" (O) =OH4W",
	"(OF) =AHV",
	" (OH) =OW5",
	"(OROUGH)=ER4OW",
	"#:(OR) =ER",
	"#:(ORS) =ERZ",
	"(OR)=AOR",
	" (ONE)=WAHN",
	"#(ONE) =WAHN",
	"(OW)=OW",
	" (OVER)=OW5VER",
	"PR(O)V=UW4",
	"(OV)=AH4V",
	"(O)^%=OW5",
	"(O)^EN=OW",
	"(O)^I#=OW5",
	"(OL)D=OW4L",
	"(OUGHT)=AO5T",
	"(OUGH)=AH5F",
	" (OU)=AW",
	"H(OU)S#=AW4",
	"(OUS)=AXS",
	"(OUR)=OHR",
	"(OULD)=UH5D",
	"(OU)^L=AH5",
	"(OUP)=UW5P",
	"(OU)=AW",
	"(OY)=OY",
	"(OING)=OW4IHNX",
	"(OI)=OY5",
	"(OOR)=OH5R",
	"(OOK)=UH5K",
	"F(OOD)=UW5D",
	"L(OOD)=AH5D",
	"M(OOD)=UW5D",
	"(OOD)=UH5D",
	"F(OOT)=UH5T",
	"(OO)=UW5",
	"(O')=OH",
	"(O)E=OW",
	"(O) =OW",
	"(OA)=OW4",
	" (ONLY)=OW4NLIY",
	" (ONCE)=WAH4NS",
	"(ON'T)=OW4NT",
	"C(O)N=AA",
	"(O)NG=AO",
	" :^(O)N=AH",
	"I(ON)=UN",
	"#:(ON)=UN",
	"#^(ON)=UN",
	"(O)ST=OW",
	"(OF)^=AO4F",
	"(OTHER)=AH5DHER",
	"R(O)B=RAA",
	"^R(O):#=OW5",
	"(OSS) =AO5S",
	"#:^(OM)=AHM",
	"(O)=AA",

	"]P",
	" (P) =PIY4",
	"(PH)=F",
	"(PEOPL)=PIY5PUL",
	"(POW)=PAW4",
	"(PUT) =PUHT",
	"(P)P=",
	"(P)S=",
	"(P)N=",
	"(PROF.)=PROHFEH4SER",
	"(P)=P",

	"]Q",
	" (Q) =KYUW4",
	"(QUAR)=KWOH5R",
	"(QU)=KW",
	"(Q)=K",

	"]R",
	" (R) =AA5R",
	" (RE)^#=RIY",
	"(R)R=",
	"(R)=R",

	"]S",
	" (S) =EH4S",
	"(SH)=SH",
	"#(SION)=ZHUN",
	"(SOME)=SAHM",
	"#(SUR)#=ZHER",
	"(SUR)#=SHER",
	"#(SU)#=ZHUW",
	"#(SSU)#=SHUW",
	"#(SED)=ZD",
	"#(S)#=Z",
	"(SAID)=SEHD",
	"^(SION)=SHUN",
	"(S)S=",
	".(S) =Z",
	"#:.E(S) =Z",
	"#:^#(S) =S",
	"U(S) =S",
	" :#(S) =Z",
	"##(S) =Z",
	" (SCH)=SK",
	"(S)C+=",
	"#(SM)=ZUM",
	"#(SN)'=ZUM",
	"(STLE)=SUL",
	"(S)=S",

	"]T",
	" (T) =TIY4",
	" (THE) #=DHIY",
	" (THE) =DHAX",
	"(TO) =TUX",
	" (THAT)=DHAET",
	" (THIS) =DHIHS",
	" (THEY)=DHEY",
	" (THERE)=DHEHR",
	"(THER)=DHER",
	"(THEIR)=DHEHR",
	" (THAN) =DHAEN",
	" (THEM) =DHAEN",
	"(THESE) =DHIYZ",
	" (THEN)=DHEHN",
	"(THROUGH)=THRUW4",
	"(THOSE)=DHOHZ",
	"(THOUGH) =DHOW",
	"(TODAY)=TUXDEY",
	"(TOMO)RROW=TUMAA5",
	"(TO)TAL=TOW5",
	" (THUS)=DHAH4S",
	"(TH)=TH",
	"#:(TED)=TIXD",
	"S(TI)#N=CH",
	"(TI)O=SH",
	"(TI)A=SH",
	"(TIEN)=SHUN",
	"(TUR)#=CHER",
	"(TU)A=CHUW",
	" (TWO)=TUW",
	"&(T)EN =",
	"(T)=T",

	"]U",
	" (U) =YUW4",
	" (UN)I=YUWN",
	" (UN)=AHN",
	" (UPON)=AXPAON",
	"@(UR)#=UH4R",
	"(UR)#=YUH4R",
	"(UR)=ER",
	"(U)^ =AH",
	"(U)^^=AH5",
	"(UY)=AY5",
	" G(U)#=",
	"G(U)%=",
	"G(U)#=W",
	"#N(U)=YUW",
	"@(U)=UW",
	"(U)=YUW",

	"]V",
	" (V) =VIY4",
	"(VIEW)=VYUW5",
	"(V)=V",

	"]W",
	" (W) =DAH4BULYUW",
	" (WERE)=WER",
	"(WA)SH=WAA",
	"(WA)ST=WEY",
	"(WA)S=WAH",
	"(WA)T=WAA",
	"(WHERE)=WHEHR",
	"(WHAT)=WHAHT",
	"(WHOL)=/HOWL",
	"(WHO)=/HUW",
	"(WH)=WH",
	"(WAR)#=WEHR",
	"(WAR)=WAOR",
	"(WOR)^=WER",
	"(WR)=R",
	"(WOM)A=WUHM",
	"(WOM)E=WIHM",
	"(WEA)R=WEH",
	"(WANT)=WAA5NT",
	"ANS(WER)=ER",
	"(W)=W",

	"]X",
	" (X) =EH4KR",
	" (X)=Z",
	"(X)=KS",

	"]Y",
	" (Y) =WAY4",
	"(YOUNG)=YAHNX",
	" (YOUR)=YOHR",
	" (YOU)=YUW",
	" (YES)=YEHS",
	" (Y)=Y",
	"F(Y)=AY",
	"PS(YCH)=AYK",
	"#:^(Y)=IY",
	"#:^(Y)I=IY",
	" :(Y) =AY",
	" :(Y)#=AY",
	" :(Y)^+:#=IH",
	" :(Y)^#=AY",
	"(Y)=IH",

	"]Z",
	" (Z) =ZIY4",
	"(Z)=Z",
	"j",
}

var rules2String = []string{
	"(A)=",
	"(!)=.",
	"(\") =-AH5NKWOWT-",
	"(\")=KWOW4T-",
	"(#)= NAH4MBER",
	"($)= DAA4LER",
	"(%)= PERSEH4NT",
	"(&)= AEND",
	"(')=",
	"(*)= AE4STERIHSK",
	"(+)= PLAH4S",
	"(,)=,",
	" (-) =-",
	"(-)=",
	"(.)= POYNT",
	"(/)= SLAE4SH",
	"(0)= ZIY4ROW",
	" (1ST)=FER4ST",
	" (10TH)=TEH4NTH",
	"(1)= WAH4N",
	" (2ND)=SEH4KUND",
	"(2)= TUW4",
	" (3RD)=THER4D",
	"(3)= THRIY4",
	"(4)= FOH4R",
	" (5TH)=FIH4FTH",
	"(5)= FAY4V",
	" (64) =SIH4KSTIY FOHR",
	"(6)= SIH4KS",
	"(7)= SEH4VUN",
	" (8TH)=EY4TH",
	"(8)= EY4T",
	"(9)= NAY4N",
	"(:)=.",
	"(;)=.",
	"(<)= LEH4S DHAEN",
	"(=)= IY4KWULZ",
	"(>)= GREY4TER DHAEN",
	"(?)=?",
	"(@)= AE6T",
	"(^)= KAE4RIXT",
	"]A",
}
```

We created a function to convert signed 4 bit two's complement numbers to signed 8 bit two's complement numbers, but it's not used, and i don't remember why. Maybe it was used before, or maybe it shouldn't have existed to begin with? :(

```
func nybbleTwosComplementToSigned(value uint8) int8 {
	if value > 0xF {
		panic("Value must be a 4-bit unsigned integer (0x0 to 0xF).")
	}

	// Convert the value to a 4-bit binary string
	binaryStr := fmt.Sprintf("%04b", value)

	// Check the sign bit (most significant bit)
	if binaryStr[0] == '0' {
		// Positive number or zero, return the value as is
		decimalValue, _ := strconv.ParseInt(binaryStr, 2, 8)
		return int8(decimalValue)
	} else {
		// Negative number, convert from two's complement
		// Invert the bits
		invertedBits := ""
		for _, bit := range binaryStr {
			if bit == '0' {
				invertedBits += "1"
			} else {
				invertedBits += "0"
			}
		}

		// Convert inverted bits to an integer
		invertedValue, _ := strconv.ParseInt(invertedBits, 2, 8)

		// Add 1 to the inverted value
		magnitude := invertedValue + 1

		// Return the negative value
		return int8(-magnitude)
	}
}
```
