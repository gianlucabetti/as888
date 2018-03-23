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
001700070330     D FIGRG00F      E DS
001800070330     D FIGRV00F      E DS
001900070330     D*
002000070330     D DS_S_GRG        DS
002100070330     D  s_grgFGS                           like(grgFGS)
002200070330     D  s_grgDDE                           like(grgDDE)
002300070330     D  s_grvKEY                           like(grvKEY)
002400070330     D*
002500070330     D Dataiso         s               d   datfmt(*iso)
002600070330     D Dataeur         s               d   datfmt(*eur)
002700050103     C**********************************************************************
002800040917     C     *ENTRY        PLIST
002900040917     C                   PARM                    KPJBA
003000050103     c*
003100040917     C                   Z-ADD     1             CODUT
003200040917     C                   CALL      'X§PARUT'
003300040917     C                   PARM                    UT§DSE0F
003400040917     C                   MOVEL     REC80         CNCR80
003500040917     C                   MOVEL     RAGUT         VIDRSU
003600070124     c
003700070125     c* Per EDP posso lanciare qualsiasi p.o.
003800070124     c                   if        %subst(knmus:1:3)='EDP'
003900070124     c                   seton                                        01
004000070124     c                   endif
004100070124     c* un secondo livello può lanciare solo se stesso
004200070124     c                   if        simtpp='2'
004300070124     c                   seton                                        02
004400070124     c                   endif
004500070125     c
004600070207     c* spedizioni in distinta: per ora a tutti
004700070125     c                   seton                                        03
004800070207     c* lancio x test: solo EDP
004900070207     c                   if        %subst(knmus:1:3)='EDP'
005000070207     c                   seton                                        04
005100070207     c                   endif
005200070330     c* Imposto linea dal p.o. utente solo se abilitato dall'organigramma
005300060905     C                   CLEAR                   V1CPO
005400041220     C                   CLEAR                   V1DPO
005500060904     C                   EVAL      V1CPO=SIMPOU
005600060912     C     V1CPO         CHAIN     AZORG01L
005700060912     C                   IF        %FOUND(AZORG01L)
005800070124     C                   MOVEL     orgdes        v1dpo
005900060912     c                   endif
006000060904     c
006100040917     c
006200040917     C     FOR01         TAG
006300000828      *
006400070330     C                   EXFMT     LRL3D01
006500970102     C                   SETOFF                                       289050
006600040917     C* F3 - FINE
006700040917     C   KC              GOTO      FINE
006800041220      *  CONTROLLI
006900040917     C                   EXSR      CTRD01
007000040917     c   90              goto      for01
007100000825      *
007200040917     c*
007300060904     c                   if        not *inkf
007400041220     c                   goto      for01
007500041220     c                   endif
007600040917     C*
007700040917      * F6  - SOTTOMETTO ELABORAZIONE
007800050412    1C     *INKF         IFEQ      *ON
007900050214     c*
008000070330     c* Esegio elaborazione
008100070330     c                   setoff                                       65
008200070330     c                   exsr      EXEGRG
008300070330     c                   setoff                                       kf
008400070330     c*
008500050412    2C                   ENDIF
008600050412     c
008700060904     c     fine          tag
008800070124     c
008900070124     c                   clear                   fnlv50ds
009000070124     c                   movel     'C'           d50tla
009100070124     c                   call      'FNLV50R'
009200070124     c                   parm                    fnlv50ds
009300000000     C                   SETON                                        LR
009400900515      *-----------------------------------------------------***********
009500060904      * CONTROLLI                                           *  CTRD01 *
009600900515      *-----------------------------------------------------***********
009700040917     C     CTRD01        BEGSR
009800070525      * FASCIA ORARIA PROTETTA (NO X EDP)
009900070525     C                   if        not *in01
010000070525     C                   time                    w0140            14 0
010100070525     C                   movel     w0140         oracor            6 0          *ORA
010200070525     C                   if        oracor > 220000 or oracor < 080000
010300070525     c                   seton                                        509028
010400070525     C                   movel     MSG(4)        V1cMSG
010500070525     c                   goto      endct1
010600070525     C                   endif
010700070525     C                   endif
010800041220      * PUNTO OPERATIVO
010900041220     C                   MOVE      V1CPO         CODFIL            3 0
011000951025     C                   EXSR      CTRFIL
011100070124    1c                   if        *in90
011200041220     c                   goto      endct1
011300041220    1C                   ENDIF
011400070124     c
011500070124     c* Verifico se  abilitato a scambio dati con tellus  se no EDP
011600070124     c                   if        not *in01
011700060904     c                   movel     orgde8        og148
011800070124     c                   if        §ogtlls=' '
011900070124     c                   seton                                        509028
012000070124     C                   MOVEL     MSG(2)        V1cMSG
012100070124     c                   goto      endct1
012200070124    1C                   ENDIF
012300070124    1C                   ENDIF
012400070330     c
012500070330     c* Verifico se la filiale indicata esistono versioni giri elaborabili
012600070330     c                   seton                                        65
012700070330     c                   exsr      EXEGRG
012800900515      *
012900040917     C     ENDCT1        ENDSR
013000951025     C**************************************************************************
013100951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
013200951025     C**************************************************************************
013300951025     C     CTRFIL        BEGSR
013400020213
013500951025     C     CODFIL        CHAIN     AZORG01L                           34
013600951025    1C  N34ORGFVA        IFNE      ' '
013700951025     C     ORGFAG        ORNE      'A'
013800951025     C     ORGFAG        ANDNE     'F'
013900951025     C                   SETON                                        34
014000951025    1C                   ENDIF
014100070124     c
014200070124    1c                   if        not *in34
014300070124     c                   movel     orgdes        V1DPO
014400070124     c
014500070124   x1C                   ELSE
014600070124     c                   seton                                        509028
014700070124     C                   MOVEL     MSG(1)        V1cMSG
014800070124     c                   goto      endctf
014900070124    1C                   ENDIF
015000070124     c
015100070124     c* Se non sono EDP deve essere p.o. della £1
015200070124     c                   if        not *in01
015300070124     C                   SETOFF                                       90
015400070124     C                   CLEAR                   fnlv50ds
015500070124     C                   MOVEL     KNMUS         D50PRU
015600070124     C                   MOVEL     codFIL        D50FGS
015700070124     C                   CALL      'FNLV50R'
015800070124     C                   PARM                    fnlv50ds
015900070124     C*
016000070124     C     D50ERR        IFNE      ' '
016100070124     C                   MOVEL     D50MSG        V1CMSG
016200070124     C                   SETON                                        509028
016300070124     C                   GOTO      ENDCTF
016400070124     C                   ENDIF
016500070124     C                   ENDIF
016600951025     C*
016700070124     C     endctf        ENDSR
016800070330     C*------------------------------------------------------------------------*
016900070330     C* EXEGRG - Routine di verifica/elaborazione filiali d cui poter scaricare l'anagrafico giri
017000070330     C*------------------------------------------------------------------------*
017100070330     C     EXEGRG        BEGSR
017200070330     C*
017300070330     C/EXEC SQL
017400070330     C+ declare C1 cursor for
017500070330     C+ select grgFGS, grgDDE, min(grvKEY)
017600070330     C+ from figrg00f join figrv00f
017700070330     C+ on grgfgs=grvfgs and grgdde=grvdde
017800070709     C+ where grgFGS = :V1CPO and grgFTT = 'S'  and
017900070709     C+ substr(grgflo, 4, 1) in ('E', 'C')
018000070330     C+ group by grgFGS, grgDDE
018100070330     C+ order by grgFGS, grgDDE
018200070330     C+ for read only
018300070330     C/END-EXEC
018400070330     C
018500070330     C/EXEC SQL
018600070330     C+ open C1
018700070330     C/END-EXEC
018800070330     C
018900070330     C/EXEC SQL
019000070330     C+ Fetch next from C1 into :ds_s_grg
019100070330     C/END-EXEC
019200070330     C*
019300070330     C* Se reperito qualcosa => analizzo
019400070330     C                   if        sqlcod = *zeros
019500070330     C                   if        *inkf = *on AND *in65 = *off
019600070330     C                   dow       sqlcod = *zeros
019700070330     C*
019800070330     C* Genero il file anagrafico giri x TELLUS della filiale corrente
019900070330     C                   exsr      OUTFIL
020000070330     C*
020100070330     C* Leggo la prossima filiale d cui generare il file anagrafico giri
020200070330     C/EXEC SQL
020300070330     C+ Fetch next from C1 into :ds_s_grg
020400070330     C/END-EXEC
020500070330     C                   enddo
020600070330     C                   endif
020700070330     C*
020800070330     C                   else
020900070330     c                   seton                                        509028
021000070330     C                   MOVEL     MSG(3)        V1cMSG
021100070330     C                   endif
021200070330     C*
021300070330     C/EXEC SQL
021400070330     C+ close C1
021500070330     C/END-EXEC
021600070330     C*
021700070330     C                   ENDSR
021800070330     C*------------------------------------------------------------------------*
021900070330     C* OUTFIL - Routine di generazione file anagrafico giri x TELLUS
022000070330     C*------------------------------------------------------------------------*
022100070330     C     OUTFIL        BEGSR
022200070330     C*
022300070330     c                   call      'FNLRL4R'
022400070330     c                   parm                    V1CTST
022500070330     c                   parm                    s_grgFGS
022600070330     c                   parm                    s_grgDDE
022700070330     c                   parm                    s_grvKEY
022800070330     C*
022900070330     C                   ENDSR
023000070330     C*--------------------------------------------------------------------------------------------*
023100070330     C* *inzsr - operazioni iniziali
023200070330     C*--------------------------------------------------------------------------------------------*
023300070330     C     *inzsr        BEGSR
023400070330     c*
023500070330     c* Data corrente
023600070330     c                   time                    w0140            14 0
023700070330     c                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
023800070330     c     *eur          movel     w0080         dataeur
023900070330     c                   movel     dataeur       dataiso
024000070330     c     *iso          movel     dataiso       datcor            8 0
024100070330     C*
024200070330     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
024300070330     C
024400070330     C/EXEC SQL
024500070330     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
024600070330     C/END-EXEC
024700070330     C*
024800070330     C                   ENDSR
024900020805      **************************************************************************
025000960302** SCHIERA MSG - MESSAGGI DI ERRORE
025100070330Filiale INESISTENTE                                                           01
025200070330Filiale non abilitata allo scambio dati con TELLUS !!!                        02
025300070330Non presenti giri da generare per la filiale indicata                         03
025400070525Richiesta in fascia oraria non consentita !!!                                 04
