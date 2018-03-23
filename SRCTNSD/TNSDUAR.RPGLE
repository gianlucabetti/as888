000100020528     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020528      *------------------------------------------------------------------------*
000300020528      * STAMPA BOLLE PER FILIALE - PARAMETRI                                   *
000400020528      *------------------------------------------------------------------------*
000500941115     FTNSDUAD   CF   E             WORKSTN
000600941114     D*------------------------------------------------------------------------*
000700941114     D* INPUT
000800941114     D*------------------------------------------------------------------------*
000900020717
001000020717     d Msg             s             78    Dim(5) Ctdata Perrcd(1)              Msg video
001100941114     D*----------------
001200941114     D* ARCHITETTURA
001300941114     D*----------------
001400941114     D KPJBA         E DS
001500941114     D*----------------
001600941114     D* PARAMETRI LANCIO
001700941114     D*----------------
001800000000     D PARAM           DS
001900941114     D  PARMAG                 1      3  0
002000941114     D  PARDAT                 4     11  0
002100941114     D  PARRIS                12     12
002200100915     D  PARncl                13     17  0 inz
002300100915     D  PARpkg                18     24  1 inz
002400100915     D  PARMAUT               25     25    inz
002500100915     D  PARcuf                26     28    inz
002600101028     D  PARaum                29     29    inz
002700131118     D  PARbol                30     30    inz('N')
002800140218     d* - parametri da TRUL90R:
002900140218     d* · pgm.   per stampa LDV
003000140218     d  wD90psl              157    166    inz
003100140218     d* · stampante per LDV in formato "A4" (laser)
003200140218     d  wD90prb4             167    176
003300140218     d* · stampante per LDV in formato "A5" (laser)
003400140218     d  wD90prb5             177    186
003500140218     d* · modulo per stampa LDV su "A4"     (laser)
003600140218     d  wD90mdb4             187    196
003700140218     d* · modulo per stampa LDV su "A5"     (laser)
003800140218     d  wD90mdb5             197    206
003900140218     d* · pgm di stampa etichette segnacolli
004000140218     d  wD90pss              207    216
004100140218     d* · modulo per stampa segnacolli
004200140218     d  wD90mde              227    236    inz('ETICH')
004300140218     d* · stampante per segnacolli
004400140218     d  wD90pre              247    256    inz('QPRINTS   ')
004500140218     d
004600000105     D*---------------------
004700000105     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000105     D*---------------------
004900000105     D WLBDA8          DS                  INZ
005000000105     D  G08DAT                 1      8  0
005100000105     D  G08INV                 9     16  0
005200000105     D  G08ERR                17     17
005300000105     D  G08TGI                18     22  0
005400020709
005500020709     d ddatiute      e ds
005600020709     d azuteds       e ds                  extname(azute00f)
005700020709     d tibs34ds      e ds                  inz
005800100930     d tnsdmagds     e ds                  inz
005900100930     d
006000100930     d wmag            s              3
006100100930     d wdmag           s             25
006200941114     C*------------------------------------------------------------------------*
006300941114     C* MAIN LINE
006400941114     C*------------------------------------------------------------------------*
006500941114     C*
006600941114     C* RICEVIMENTO PARAMETRI
006700900131     C     *ENTRY        PLIST
006800900131     C                   PARM                    KPJBA
006900971010     C*
007000971010     C* REPERISCE I DATI DELL'UTENTE
007100020709
007200020709     c     *dtaara       define    §azute        azuteds
007300020709     c     *dtaara       define    §datiute      ddatiute
007400020709
007500020709     c                   in(E)     *dtaara
007600020709     c                   if        %Error or rsut = *blanks
007700020709     c                   clear                   tibs34ds
007800020709     c                   call      'TIBS34R'
007900020709     c                   parm                    Tibs34Ds
008000020709     c                   in        *dtaara
008100020709     c                   endif
008200020717
008300020717      * Controllo il profilo utente
008400020717     c                   ExSr      CtrUte
008500941114     C*
008600941114     C* IMPOSTAZIONI VIDEO
008700941215     C     FMT01         TAG
008800941114     C                   MOVEL     *BLANKS       VICMAG
008900941114     C                   MOVEL     *BLANKS       VIDMAG
009000000105     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
009100000105     C                   MOVE      N14           VICDAT
009200941114     C                   MOVEL     'N'           VICRIS
009300941114     C*
009400971010DO  1C     *INKF         DOWEQ     '0'
009500920507     C     *INKC         ANDEQ     '0'
009600920507     C     *IN99         OREQ      '1'
009700920507     C     *INKC         ANDEQ     '0'
009800971010     C     *INKF         ANDEQ     '0'
009900941114     C                   EXFMT     SEUAD1
010000020717
010100020717      * Se Profilo non OK devo uscire dal pgm
010200020717     c                   If        *In76
010300020717     c                   Eval      *InKc = '1'
010400020717     c                   EndIf
010500020717
010600941114DO  2C  NKC              DO
010700941114     C* CONTROLLI
010800920507     C                   SETOFF                                       112526
010900920507     C                   SETOFF                                       99
011000941114     C* CODICE MAGAZZINO
011100941114     C                   MOVEL     VICMAG        WMAG
011200941114     C                   MOVEL     *BLANKS       WDMAG
011300941114     C                   EXSR      CHKMAG
011400021210     c                   if        *In99
011500021210     c                   Eval      *InKf = *Off
011600021210     c                   GoTo      Fmt01
011700021210     c                   EndIf
011800941114     C                   MOVEL     WMAG          VICMAG
011900941114     C                   MOVEL     WDMAG         VIDMAG
012000941114     C* DATA GG/MM/AAAA
012100941114     C                   Z-ADD     *ZEROS        DATVIC            8 0
012200941114IF  3C     *IN99         IFEQ      *OFF                                         *NO ERRORI
012300941114IF  4C     VICDAT        IFEQ      *ZEROS
012400941114     C                   SETON                                        2599      *DATA OBBLIGATORIA
012500941114X   4C                   ELSE
012600000105     C                   Z-ADD     VICDAT        G08DAT
012700000105     C                   Z-ADD     *ZEROS        G08INV
012800000105     C                   MOVEL     '0'           G08ERR
012900941114     C                   CALL      'XSRDA8'
013000000105     C                   PARM                    WLBDA8
013100000105IF  5C     G08ERR        IFEQ      '1'
013200941114     C                   SETON                                        2599
013300941114X   5C                   ELSE
013400000105     C                   Z-ADD     G08DAT        VICDAT
013500000105     C                   Z-ADD     G08INV        DATVIC
013600941114E   5C                   ENDIF
013700941114E   4C                   ENDIF
013800941114E   3C                   ENDIF
013900941114     C* CONTROLLO RISTAMPA
014000941114IF  3C     *IN99         IFEQ      *OFF
014100941114IF  4C     VICRIS        IFEQ      'S'
014200941114     C     VICRIS        OREQ      'N'
014300941114X   4C                   ELSE
014400920507     C                   SETON                                        2699
014500941114E   4C                   END
014600941114E   3C                   END
014700941114     C*
014800941114E   2C                   ENDDO
014900941114     C*
015000941114     C* F01 - ELABORAZIONE
015100971010     C   KF
015200971010DO  2CANN99              DO
015300941114     C                   MOVEL     VICMAG        PARMAG
015400941114     C                   Z-ADD     DATVIC        PARDAT
015500941114     C                   MOVEL     VICRIS        PARRIS
015600900131     C                   MOVEL     PARAM         KPJBU
015700941128     C                   MOVEL     'SDUB'        KCOAZ
015800941114IF  3C     KNMUS         IFNE      *ALL'1'
015900900131     C                   CALL      'BCH10'
016000000000     C                   PARM                    KPJBA
016100941114X   3C                   ELSE
016200941115     C                   CALL      'TNSDUBR'
016300000000     C                   PARM                    KPJBA
016400941114E   3C                   ENDIF
016500971010     C                   MOVEL     '0'           *INKF
016600941215     C                   GOTO      FMT01
016700941114E   2C                   ENDDO
016800941114E   1C                   ENDDO
016900920507     C*
017000000000     C                   SETON                                        LR
017100020717      *------------------------------------------------------------------------*
017200020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
017300020717      *------------------------------------------------------------------------*
017400020717     c     CtrUte        BegSr
017500020717
017600020717     c                   Eval      *In76 = *Off
017700020717
017800020717      * Controllo se il profilo è ok da tibs34
017900020717     c                   If        DutErr = 'E'
018000020717     c                   eval      VidMsg = DutMsg
018100020717     c                   eval      *In76 = *On
018200020717     c                   endIf
018300020717
018400020717      * Controllo se il profilo di sede
018500020717     c                   If        DutLpo <> 'S'
018600020717     c                   Eval      VidMsg = Msg(1)
018700020717     c                   Eval      *In76 = *On
018800020717     c                   EndIf
018900020717
019000020717     c                   endSr
019100941114     C*------------------------------------------------------------------------*
019200941114     C* CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
019300941114     C*------------------------------------------------------------------------*
019400941114     C     CHKMAG        BEGSR
019500100930     c                   clear                   tnsdmagds
019600100930     c                   eval      §magmag=wmag
019700100930     c                   eval      §imagtorg='E  '
019800100930     c                   call      'TNSDMAGR'
019900100930     c                   parm                    kpjba
020000100930     c                   parm                    tnsdmagds
020100100930     c
020200100930     c* Eseguita Ricerca
020300100930    1c                   if        §omagerr='R'
020400100930     c                   eval      wmag=§magmag
020500100930     c                   eval      wdmag=§omagdes
020600100930   x1c                   else
020700100930     c* Errore nel magazzino
020800100930    2c                   if        §omagerr='E'
020900100930     C                   SETON                                        1199
021000100930     c                   eval      wdmag=*all'?'
021100100930     c* se tutto ok prendo decodifica e flag
021200100930   x2c                   else
021300100930     c                   eval      wdmag=§omagdes
021400100930    2c                   endif
021500100930    1c                   endif
021600100930     C*
021700941114     C*
021800941114     C                   ENDSR
021900020717** MSG
022000020717Non è Profilo non abilitato alla funzione                                           1
