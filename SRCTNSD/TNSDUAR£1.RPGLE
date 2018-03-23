000100020528     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020528      *------------------------------------------------------------------------*
000300020528      * STAMPA BOLLE PER FILIALE - PARAMETRI                                   *
000400020528      *------------------------------------------------------------------------*
000500020528     FAZORG21l  IF   E           K DISK
000600020528     fazorg01l  if   e           k disk
000700941115     FTNSDUAD   CF   E             WORKSTN
000800941114     D*------------------------------------------------------------------------*
000900941114     D* INPUT
001000941114     D*------------------------------------------------------------------------*
001100020717
001200020717     d Msg             s             78    Dim(5) Ctdata Perrcd(1)              Msg video
001300020717
001400941114     D*----------------
001500941114     D* ARCHITETTURA
001600941114     D*----------------
001700941114     D KPJBA         E DS
001800941114     D  LIBSYS                92    101
001900941114     D  MSGERR               453    502
002000941114     D*----------------
002100941114     D* PARAMETRI LANCIO
002200941114     D*----------------
002300000000     D PARAM           DS
002400941114     D  PARMAG                 1      3  0
002500941114     D  PARDAT                 4     11  0
002600941114     D  PARRIS                12     12
002700000105     D*---------------------
002800000105     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900000105     D*---------------------
003000000105     D WLBDA8          DS                  INZ
003100000105     D  G08DAT                 1      8  0
003200000105     D  G08INV                 9     16  0
003300000105     D  G08ERR                17     17
003400000105     D  G08TGI                18     22  0
003500020709
003600020709     d ddatiute      e ds
003700020709     d azuteds       e ds                  extname(azute00f)
003800020709     d tibs34ds      e ds                  inz
003900941114     C*------------------------------------------------------------------------*
004000941114     C* MAIN LINE
004100941114     C*------------------------------------------------------------------------*
004200941114     C*
004300941114     C* RICEVIMENTO PARAMETRI
004400900131     C     *ENTRY        PLIST
004500900131     C                   PARM                    KPJBA
004600971010     C*
004700971010     C* REPERISCE I DATI DELL'UTENTE
004800020709
004900020709     c     *dtaara       define    §azute        azuteds
005000020709     c     *dtaara       define    §datiute      ddatiute
005100020709
005200020709     c                   in(E)     *dtaara
005300020709     c                   if        %Error or rsut = *blanks
005400020709     c                   clear                   tibs34ds
005500020709     c                   call      'TIBS34R'
005600020709     c                   parm                    Tibs34Ds
005700020709     c                   in        *dtaara
005800020709     c                   endif
005900020717
006000020717      * Controllo il profilo utente
006100020717     c                   ExSr      CtrUte
006200941114     C*
006300941114     C* DEFINIZIONE CHIAVI DI LETTURA
006400941114     C     *LIKE         DEFINE    ORGFIL        KORG
006500941114     C     KEYORG        KLIST
006600941114     C                   KFLD                    KORG
006700941114     C* IMPOSTAZIONI VIDEO
006800941215     C     FMT01         TAG
006900941114     C                   MOVEL     *BLANKS       VICMAG
007000941114     C                   MOVEL     *BLANKS       VIDMAG
007100000105     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
007200000105     C                   MOVE      N14           VICDAT
007300941114     C                   MOVEL     'N'           VICRIS
007400941114     C*
007500971010DO  1C     *INKF         DOWEQ     '0'
007600920507     C     *INKC         ANDEQ     '0'
007700920507     C     *IN99         OREQ      '1'
007800920507     C     *INKC         ANDEQ     '0'
007900971010     C     *INKF         ANDEQ     '0'
008000941114     C                   EXFMT     SEUAD1
008100020717
008200020717      * Se Profilo non OK devo uscire dal pgm
008300020717     c                   If        *In76
008400020717     c                   Eval      *InKc = '1'
008500020717     c                   EndIf
008600020717
008700941114DO  2C  NKC              DO
008800941114     C* CONTROLLI
008900920507     C                   SETOFF                                       112526
009000920507     C                   SETOFF                                       99
009100941114     C* CODICE MAGAZZINO
009200941114     C                   MOVEL     VICMAG        WMAG
009300941114     C                   MOVEL     *BLANKS       WDMAG
009400941114     C                   EXSR      CHKMAG
009500021210     c                   if        *In99
009600021210     c                   Eval      *InKf = *Off
009700021210     c                   GoTo      Fmt01
009800021210     c                   EndIf
009900941114     C                   MOVEL     WMAG          VICMAG
010000941114     C                   MOVEL     WDMAG         VIDMAG
010100941114     C* DATA GG/MM/AAAA
010200941114     C                   Z-ADD     *ZEROS        DATVIC            8 0
010300941114IF  3C     *IN99         IFEQ      *OFF                                         *NO ERRORI
010400941114IF  4C     VICDAT        IFEQ      *ZEROS
010500941114     C                   SETON                                        2599      *DATA OBBLIGATORIA
010600941114X   4C                   ELSE
010700000105     C                   Z-ADD     VICDAT        G08DAT
010800000105     C                   Z-ADD     *ZEROS        G08INV
010900000105     C                   MOVEL     '0'           G08ERR
011000941114     C                   CALL      'XSRDA8'
011100000105     C                   PARM                    WLBDA8
011200000105IF  5C     G08ERR        IFEQ      '1'
011300941114     C                   SETON                                        2599
011400941114X   5C                   ELSE
011500000105     C                   Z-ADD     G08DAT        VICDAT
011600000105     C                   Z-ADD     G08INV        DATVIC
011700941114E   5C                   ENDIF
011800941114E   4C                   ENDIF
011900941114E   3C                   ENDIF
012000941114     C* CONTROLLO RISTAMPA
012100941114IF  3C     *IN99         IFEQ      *OFF
012200941114IF  4C     VICRIS        IFEQ      'S'
012300941114     C     VICRIS        OREQ      'N'
012400941114X   4C                   ELSE
012500920507     C                   SETON                                        2699
012600941114E   4C                   END
012700941114E   3C                   END
012800941114     C*
012900941114E   2C                   ENDDO
013000941114     C*
013100941114     C* F01 - ELABORAZIONE
013200971010     C   KF
013300971010DO  2CANN99              DO
013400941114     C                   MOVEL     VICMAG        PARMAG
013500941114     C                   Z-ADD     DATVIC        PARDAT
013600941114     C                   MOVEL     VICRIS        PARRIS
013700900131     C                   MOVEL     PARAM         KPJBU
013800941128     C                   MOVEL     'SDUB'        KCOAZ
013900941114IF  3C     KNMUS         IFNE      *ALL'1'
014000900131     C                   CALL      'BCH10'
014100000000     C                   PARM                    KPJBA
014200941114X   3C                   ELSE
014300941115     C                   CALL      'TNSDUBR'
014400000000     C                   PARM                    KPJBA
014500941114E   3C                   ENDIF
014600971010     C                   MOVEL     '0'           *INKF
014700941215     C                   GOTO      FMT01
014800941114E   2C                   ENDDO
014900941114E   1C                   ENDDO
015000920507     C*
015100000000     C                   SETON                                        LR
015200020717      *------------------------------------------------------------------------*
015300020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
015400020717      *------------------------------------------------------------------------*
015500020717     c     CtrUte        BegSr
015600020717
015700020717     c                   Eval      *In76 = *Off
015800020717
015900020717      * Controllo se il profilo è ok da tibs34
016000020717     c                   If        DutErr = 'E'
016100020717     c                   eval      VidMsg = DutMsg
016200020717     c                   eval      *In76 = *On
016300020717     c                   endIf
016400020717
016500020717      * Controllo se il profilo di sede
016600020717     c                   If        DutLpo <> 'S'
016700020717     c                   Eval      VidMsg = Msg(1)
016800020717     c                   Eval      *In76 = *On
016900020717     c                   EndIf
017000020717
017100020717     c                   endSr
017200941114     C*------------------------------------------------------------------------*
017300941114     C* CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
017400941114     C*------------------------------------------------------------------------*
017500941114     C     CHKMAG        BEGSR
017600941114     C*
017700941114     C* CONTROLLO SE E' STATO INSERITO IL '?'
017800941114     C                   SETOFF                                           21
017900941114     C     '?'           SCAN      WMAG                                   21
018000941114     C*
018100941114     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
018200941114IF  1C     *IN21         IFEQ      '1'
018300941114     C                   SETON                                        99        X RIEMETTERE VIDEO
018400941114     C                   MOVEL     *BLANKS       §COD1
018500941114     C                   MOVE      *BLANKS       §DES1
018600941222     C                   CALL      'TNSD20R'
018700941114     C                   PARM                    §COD1             3
018800941114     C                   PARM                    §TIP              1
018900941114     C                   PARM                    §DES1            25
019000941114     C                   MOVEL     §COD1         WMAG              3
019100941114     C                   MOVEL     §DES1         WDMAG            25
019200941114     C* NON E' STATO INSERITO '?'
019300941114X   1C                   ELSE
019400941114     C*
019500941114     C* INSERITI CARATTERI
019600941114IF  2C     WMAG          IFLT      *ZEROS
019700941114     C                   SETON                                        9911
019800941114X   2C                   ELSE
019900941114     C*
020000941114     C* INSERITO CODICE: CONTROLLO IN ANAGRAFICA
020100941114     C                   MOVEL     WMAG          KORG
020200020531     C     KEYORG        CHAIN     AZORG21l
020300020531IF  3C                   IF        %found(azorg21l)
020400941114     C                   MOVEL     ORGFIL        WMAG
020500941114     C                   MOVEL     ORGDES        WDMAG
020600941114X   3C                   ELSE
020700020528     c     keyorg        chain     azorg01l
020800020528     c                   if        %found(azorg01l)
020900020528     c                   movel     orgfil        wmag
021000020528     c                   movel     orgdes        wdmag
021100020528     c                   else
021200021210     C                   SETON                                        1199
021300941114     C                   MOVEL     *ALL'*'       WDMAG
021400020528     c                   endif
021500941114E   3C                   ENDIF
021600941114E   2C                   ENDIF
021700941114E   1C                   ENDIF
021800941114     C*
021900941114     C                   ENDSR
022000020717** MSG
022100020717Non è Profilo non abilitato alla funzione                                           1
