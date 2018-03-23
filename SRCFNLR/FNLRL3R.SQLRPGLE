000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200070330      * FNLRL3R *----------------------------------------------------*
000300070330      *         - GENERA ANAGRAFICO BOLLE X TELLUS - DOWNLOAD
000400940915      *--------------------------------------------------------------*
000500070330     FFNLRL3D   CF   E             WORKSTN
000600951013     FAZORG01L  IF   E           K DISK
000700020805      *
000800020805      * DEFINIZIONE SCHIERE
000900960302     D*
001000070525     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001100070330     D*
001200040917     D KPJBA         E DS
001300040917     D CNCR80        E DS
001400060904     D og148         E DS
001500040917     D UT§DSE0F      E DS
001600070124     D fnlv50ds      E DS
001700170823     D fnlv55ds      E DS
001800070330     D FIGRG00F      E DS
001900070330     D FIGRV00F      E DS
002000070330     D*
002100070330     D DS_S_GRG        DS
002200070330     D  s_grgFGS                           like(grgFGS)
002300070330     D  s_grgDDE                           like(grgDDE)
002400070330     D  s_grvKEY                           like(grvKEY)
002500070330     D*
002600070330     D Dataiso         s               d   datfmt(*iso)
002700070330     D Dataeur         s               d   datfmt(*eur)
002800050103     C**********************************************************************
002900170823     C*
003000170823     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
003100170823     C
003200170823     C/EXEC SQL
003300170823     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
003400170823     C/END-EXEC
003500170823     C
003600040917     C     *ENTRY        PLIST
003700040917     C                   PARM                    KPJBA
003800050103     c*
003900040917     C                   Z-ADD     1             CODUT
004000040917     C                   CALL      'X§PARUT'
004100040917     C                   PARM                    UT§DSE0F
004200040917     C                   MOVEL     REC80         CNCR80
004300040917     C                   MOVEL     RAGUT         VIDRSU
004400070124     c
004500070125     c* Per EDP posso lanciare qualsiasi p.o.
004600070124     c                   if        %subst(knmus:1:3)='EDP'
004700070124     c                   seton                                        01
004800070124     c                   endif
004900070124     c* un secondo livello può lanciare solo se stesso
005000070124     c                   if        simtpp='2'
005100070124     c                   seton                                        02
005200070124     c                   endif
005300070125     c
005400070207     c* spedizioni in distinta: per ora a tutti
005500070125     c                   seton                                        03
005600070207     c* lancio x test: solo EDP
005700070207     c                   if        %subst(knmus:1:3)='EDP'
005800070207     c                   seton                                        04
005900070207     c                   endif
006000070330     c* Imposto linea dal p.o. utente solo se abilitato dall'organigramma
006100060905     C                   CLEAR                   V1CPO
006200041220     C                   CLEAR                   V1DPO
006300060904     C                   EVAL      V1CPO=SIMPOU
006400060912     C     V1CPO         CHAIN     AZORG01L
006500060912     C                   IF        %FOUND(AZORG01L)
006600070124     C                   MOVEL     orgdes        v1dpo
006700060912     c                   endif
006800060904     c
006900040917     c
007000040917     C     FOR01         TAG
007100000828      *
007200070330     C                   EXFMT     LRL3D01
007300970102     C                   SETOFF                                       289050
007400040917     C* F3 - FINE
007500040917     C   KC              GOTO      FINE
007600041220      *  CONTROLLI
007700040917     C                   EXSR      CTRD01
007800040917     c   90              goto      for01
007900000825      *
008000040917     c*
008100060904     c                   if        not *inkf
008200041220     c                   goto      for01
008300041220     c                   endif
008400040917     C*
008500040917      * F6  - SOTTOMETTO ELABORAZIONE
008600050412    1C     *INKF         IFEQ      *ON
008700050214     c*
008800070330     c* Esegio elaborazione
008900070330     c                   setoff                                       65
009000070330     c                   exsr      EXEGRG
009100070330     c                   setoff                                       kf
009200070330     c*
009300050412    2C                   ENDIF
009400050412     c
009500060904     c     fine          tag
009600070124     c
009700070124     c                   clear                   fnlv50ds
009800070124     c                   movel     'C'           d50tla
009900070124     c                   call      'FNLV50R'
010000070124     c                   parm                    fnlv50ds
010100170823     c
010200170823     c                   clear                   fnlv55ds
010300170823     c                   movel     'C'           d55tla
010400170823     c                   call      'FNLV55R'
010500170823     c                   parm                    fnlv55ds
010600170823     c
010700000000     C                   SETON                                        LR
010800900515      *-----------------------------------------------------***********
010900060904      * CONTROLLI                                           *  CTRD01 *
011000900515      *-----------------------------------------------------***********
011100040917     C     CTRD01        BEGSR
011200070525      * FASCIA ORARIA PROTETTA (NO X EDP)
011300070525     C                   if        not *in01
011400070525     C                   time                    w0140            14 0
011500070525     C                   movel     w0140         oracor            6 0          *ORA
011600070525     C                   if        oracor > 220000 or oracor < 080000
011700070525     c                   seton                                        509028
011800070525     C                   movel     MSG(4)        V1cMSG
011900070525     c                   goto      endct1
012000070525     C                   endif
012100070525     C                   endif
012200041220      * PUNTO OPERATIVO
012300041220     C                   MOVE      V1CPO         CODFIL            3 0
012400951025     C                   EXSR      CTRFIL
012500070124    1c                   if        *in90
012600041220     c                   goto      endct1
012700041220    1C                   ENDIF
012800070124     c
012900070124     c* Verifico se  abilitato a scambio dati con tellus  se no EDP
013000070124     c                   if        not *in01
013100060904     c                   movel     orgde8        og148
013200070124     c                   if        §ogtlls=' '
013300070124     c                   seton                                        509028
013400070124     C                   MOVEL     MSG(2)        V1cMSG
013500070124     c                   goto      endct1
013600070124    1C                   ENDIF
013700070124    1C                   ENDIF
013800070330     c
013900070330     c* Verifico se la filiale indicata esistono versioni giri elaborabili
014000070330     c                   seton                                        65
014100070330     c                   exsr      EXEGRG
014200900515      *
014300040917     C     ENDCT1        ENDSR
014400951025     C**************************************************************************
014500951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
014600951025     C**************************************************************************
014700951025     C     CTRFIL        BEGSR
014800020213
014900951025     C     CODFIL        CHAIN     AZORG01L                           34
015000951025    1C  N34ORGFVA        IFNE      ' '
015100951025     C     ORGFAG        ORNE      'A'
015200951025     C     ORGFAG        ANDNE     'F'
015300951025     C                   SETON                                        34
015400951025    1C                   ENDIF
015500070124     c
015600070124    1c                   if        not *in34
015700070124     c                   movel     orgdes        V1DPO
015800070124     c
015900070124   x1C                   ELSE
016000070124     c                   seton                                        509028
016100070124     C                   MOVEL     MSG(1)        V1cMSG
016200070124     c                   goto      endctf
016300070124    1C                   ENDIF
016400070124     c
016500070124     c* Se non sono EDP deve essere p.o. della £1
016600070124     c                   if        not *in01
016700070124     C                   SETOFF                                       90
016800070124     C                   CLEAR                   fnlv50ds
016900070124     C                   MOVEL     KNMUS         D50PRU
017000070124     C                   MOVEL     codFIL        D50FGS
017100070124     C                   CALL      'FNLV50R'
017200070124     C                   PARM                    fnlv50ds
017300070124     C*
017400170823     C                   SELECT
017500170823     C                   WHEN      D50ERR = '2'
017600170823     C                   CLEAR                   fnlv55ds
017700170823     C                   EVAL      D55LIN = CODFIL
017800170823     C                   EVAL      D55TPT = '6'
017900170823     C                   EVAL      D55DRF = datcor
018000170823     C                   CALL      'FNLV55R'
018100170823     C                   PARM                    fnlv55ds
018200170823     C                   IF        D55TFA = SIMPOU
018300170823     C                   ELSE
018400170823     C                   MOVEL     D50MSG        V1CMSG
018500170823     C                   SETON                                        509028
018600170823     C                   GOTO      ENDCTF
018700170823     C                   ENDIF
018800170823     C*
018900170823     C                   WHEN      D50ERR <> ' '
019000070124     C                   MOVEL     D50MSG        V1CMSG
019100070124     C                   SETON                                        509028
019200070124     C                   GOTO      ENDCTF
019300170823     C                   ENDSL
019400170823     C*
019500070124     C                   ENDIF
019600951025     C*
019700070124     C     endctf        ENDSR
019800070330     C*------------------------------------------------------------------------*
019900070330     C* EXEGRG - Routine di verifica/elaborazione filiali d cui poter scaricare l'anagrafico giri
020000070330     C*------------------------------------------------------------------------*
020100070330     C     EXEGRG        BEGSR
020200070330     C*
020300070330     C/EXEC SQL
020400070330     C+ declare C1 cursor for
020500070330     C+ select grgFGS, grgDDE, min(grvKEY)
020600070330     C+ from figrg00f join figrv00f
020700070330     C+ on grgfgs=grvfgs and grgdde=grvdde
020800070709     C+ where grgFGS = :V1CPO and grgFTT = 'S'  and
020900070709     C+ substr(grgflo, 4, 1) in ('E', 'C')
021000070330     C+ group by grgFGS, grgDDE
021100070330     C+ order by grgFGS, grgDDE
021200070330     C+ for read only
021300070330     C/END-EXEC
021400070330     C
021500070330     C/EXEC SQL
021600070330     C+ open C1
021700070330     C/END-EXEC
021800070330     C
021900070330     C/EXEC SQL
022000070330     C+ Fetch next from C1 into :ds_s_grg
022100070330     C/END-EXEC
022200070330     C*
022300070330     C* Se reperito qualcosa => analizzo
022400070330     C                   if        sqlcod = *zeros
022500070330     C                   if        *inkf = *on AND *in65 = *off
022600070330     C                   dow       sqlcod = *zeros
022700070330     C*
022800070330     C* Genero il file anagrafico giri x TELLUS della filiale corrente
022900070330     C                   exsr      OUTFIL
023000070330     C*
023100070330     C* Leggo la prossima filiale d cui generare il file anagrafico giri
023200070330     C/EXEC SQL
023300070330     C+ Fetch next from C1 into :ds_s_grg
023400070330     C/END-EXEC
023500070330     C                   enddo
023600070330     C                   endif
023700070330     C*
023800070330     C                   else
023900070330     c                   seton                                        509028
024000070330     C                   MOVEL     MSG(3)        V1cMSG
024100070330     C                   endif
024200070330     C*
024300070330     C/EXEC SQL
024400070330     C+ close C1
024500070330     C/END-EXEC
024600070330     C*
024700070330     C                   ENDSR
024800070330     C*------------------------------------------------------------------------*
024900070330     C* OUTFIL - Routine di generazione file anagrafico giri x TELLUS
025000070330     C*------------------------------------------------------------------------*
025100070330     C     OUTFIL        BEGSR
025200070330     C*
025300070330     c                   call      'FNLRL4R'
025400070330     c                   parm                    V1CTST
025500070330     c                   parm                    s_grgFGS
025600070330     c                   parm                    s_grgDDE
025700070330     c                   parm                    s_grvKEY
025800070330     C*
025900070330     C                   ENDSR
026000070330     C*--------------------------------------------------------------------------------------------*
026100070330     C* *inzsr - operazioni iniziali
026200070330     C*--------------------------------------------------------------------------------------------*
026300070330     C     *inzsr        BEGSR
026400070330     c*
026500070330     c* Data corrente
026600070330     c                   time                    w0140            14 0
026700070330     c                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
026800070330     c     *eur          movel     w0080         dataeur
026900070330     c                   movel     dataeur       dataiso
027000070330     c     *iso          movel     dataiso       datcor            8 0
027100070330     C*
027200070330     C                   ENDSR
027300020805      **************************************************************************
027400960302** SCHIERA MSG - MESSAGGI DI ERRORE
027500070330Filiale INESISTENTE                                                           01
027600070330Filiale non abilitata allo scambio dati con TELLUS !!!                        02
027700070330Non presenti giri da generare per la filiale indicata                         03
027800070525Richiesta in fascia oraria non consentita !!!                                 04
