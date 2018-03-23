000100040804     H DECEDIT('0,') DATEDIT(*ymd.)
000200170207     H* fits42r *----------------------------------------------------*
000300170202     H*   bolle partenze e arrivi + variazione + EVS + ARI
000400040804     H*--------------------------------------------------------------*
000401170202     FFNBLPnew  iF a E           K DISK
000402170202     F                                     RENAME(FNBLP000:FNBLPN)
000500040804     FFNARBNEW  iF a E           K DISK
000600040804     F                                     RENAME(FNARB000:FNARBn)
000700060224     FFiAR4NEW  if a E           K DISK
000800060224     F                                     RENAME(FiAR4000:FiAR4n)
000801180205     FFipndNEW  if a E           K DISK
000802180205     F                                     RENAME(Fipnd000:Fipndn)
000900040804     FFIAR5NEW  iF a E           K DISK
001000040804     F                                     RENAME(FIAR5000:FIAR5n)
001100040804     FFIAR6NEW  iF a E           K DISK
001200040804     F                                     RENAME(FIAR6000:FIAR6n)
001300040804     FFIAR7NEW  iF a E           K DISK
001400040804     F                                     RENAME(FIAR7000:FIAR7n)
001500080228     FFIAR8NEW  iF a E           K DISK
001600080228     F                                     RENAME(FIAR8000:FIAR8n)
001700051118     FFiAR9NEW  iF a E           K DISK
001800051118     F                                     RENAME(FiAR9000:FiAR9n)
001900070424     FFiARPNEW  iF a E           K DISK
002000070424     F                                     RENAME(FiARP000:FiARPn)
002100040804     FFNARTNEW  o  a E           K DISK
002200040804     F                                     RENAME(FNART000:FNARTn)
002201170202     FFiARTNEW  o  a E           K DISK
002202170202     F                                     RENAME(FiART000:FiARTn)
002203170210     f                                     prefix(n)
002204170202     FFNBLTnew  o  a E           K DISK
002205170202     F                                     RENAME(FNBLt000:FNBLTN)
002300051014     FFIARSNEW  iF a E           K DISK
002400051014     F                                     RENAME(FIARS000:FIARSn)
002500060224     FFIdp5NEW  iF a E           K DISK
002600060224     F                                     RENAME(FIdp5000:FIdp5n)
002700040804     FFNEVSNEW  o  a E           K DISK
002800040804     F                                     RENAME(FNEVS000:FNEVSn)
002801180205     FFNBLDnew  o  a E           K DISK
002802180205     F                                     RENAME(FNBLD000:FNBLDN)
002900040804     FFIARBFNW  iF   E           K DISK
003000040804     F                                     RENAME(FNARBD00:FNARBD1)
003100040804     F                                     RENAME(FNARBK00:FNARBK1)
003200040804     F                                     RENAME(FIARBT00:FIARBT1)
003300040804     F                                     RENAME(FNARBG00:FNARBG1)
003400040804     F                                     RENAME(FNARBM00:FNARBM1)
003500040804     F                                     RENAME(FNARBV00:FNARBV1)
003600040804     F                                     RENAME(FNARBP00:FNARBP1)
003700131011     F                                     RENAME(FNARBN00:FNARBN1)
003800040804     FFIARBUNW  o  a E           K DISK
003900040804     F                                     RENAME(FIARBU00:FIARBUn)
004000040804     FFIARINEW  o  a E           K DISK
004100040804     F                                     RENAME(FIARI000:FIARIn)
004200950622     F*
004300061114     FFIARGNEW  o  a E           K DISK
004400061114     F                                     RENAME(FIARG000:FIARGn)
004500070424     FFIARNNEW  o  a E           K DISK
004600070424     F                                     RENAME(FIARn000:FIARNn)
004700061114     F*
004800040804     FFNARBDnw  O  A E             DISK
004900040804     F                                     RENAME(FNARBD00:FNARBDN)
005000040804     FFNARBKnw  O  A E             DISK
005100040804     F                                     RENAME(FNARBK00:FNARBKN)
005200040804     FFIARBTnw  O  A E             DISK
005300040804     F                                     RENAME(FIARBT00:FIARBTN)
005400040804     FFNARBGnw  O  A E             DISK
005500040804     F                                     RENAME(FNARBG00:FNARBGN)
005600040804     FFNARBMnw  O  A E             DISK
005700040804     F                                     RENAME(FNARBM00:FNARBMN)
005800040804     FFNARBVnw  O  A E             DISK
005900040804     F                                     RENAME(FNARBV00:FNARBVN)
006000040804     FFNARBPnw  O  A E             DISK
006100040804     F                                     RENAME(FNARBP00:FNARBPN)
006200131011     FFNARBNnw  O  A E             DISK
006300131011     F                                     RENAME(FNARBN00:FNARBNN)
006400040805     FFNEVBnew  iF a E           K DISK
006500040805     F                                     RENAME(FNEVB000:FNEVBN) prefix(£)
006600070430     FFNEVB2nw  iF a E           K DISK
006700070430     F                                     RENAME(FNEVB200:FNEVB2N) prefix(£)
006800040805     fFnlblNEW  if a e           k disk
006900040805     F                                     RENAME(FNlbl000:FNlbln)
007000040915     fEDRDENEW  if a e           k disk
007100040915     F                                     RENAME(EDRDE000:RDENEW)
007200040804     f*
007400060208     FFNARB01l  IF   E           K DISK
007501170209     FFNblp01l  IF   E           K DISK
007502170209     F                                     RENAME(fnblp000:fnblp001)
007503170202     FFNBLP37L  iF   E           K DISK
007504170209     F                                     RENAME(fnblp000:fnblp036)
007505170207     FFNBLP26L  iF   E           K DISK
007600131011     FFIARBF2C  IF   E           K DISK
007700040804     FFIARBu1l  IF   E           K DISK
007800060224     FFiAR401l  IF   E           k DISK
007801180205     FFipnd01l  IF   E           k DISK
007900040804     FFIAR501L  IF   E           K DISK
008000040804     FFIAR601L  iF   E           K DISK
008100040804     FFIAR701L  iF   E           K DISK
008200080228     FFIAR801L  iF   E           K DISK
008300051118     FFiAR901l  if   E           k DISK
008400170202     FFNBLT01l  if   E           k DISK
008401170202     FFNART01l  if   E           k DISK
008402170210     FFIART01l  if   E           k DISK    prefix(n)
008500051014     FFIARS01l  if   E           k DISK
008501180205     FFNBLD01L  iF   E           K DISK
008600040804     FFNEVS01l  if   E           k DISK
008700060224     FFidp501r  if   E           k DISK
008800040804     FFNEVS03l  if   E           k DISK
008900040804     F                                     RENAME(FNevs000:FNevs003)
009000040804     FFIARI01l  if   E           k DISK
009100061114     FFIARG01l  if   E           k DISK
009200070424     FFIARP01l  if   E           k DISK
009300070424     FFIARN01l  if   E           k DISK
009400040805     fFNEVB01L  if   e           k disk
009500070430     fFNEVB22L  if   e           k disk
009600040805     fFnlbl01L  if   e           k disk
009700040805     fFnlbl02L  if   e           k disk
009800040805     F                                     RENAME(FNlbl000:FNlbl002)
009900040915     fEDRDE01L  if   e           k disk
010000901006     FSYSPRT    O    F  132        PRINTER OFLIND(*INOF)
010100031124
010200031124     d ol_ArbCbo       s                   Like(ArbCbo)
010300031124     d ol_ArbCvb       s                   Like(ArbCvb)
010400031124     d ol_ArbVlc       s                   Like(ArbVlc)
010500031124     d ol_ArbFvc       s                   Like(ArbFvc)
010600031124     d ol_ArbNcr       s                   Like(ArbNcr)
010700031124     d okvol           s              1
010800040322     d ol_ArbTcr       s                   like(ArbTcr)
010900040322     d ol_ArbDcr       s                   like(ArbDcr)
011000040322     d ol_ArbHcr       s                   like(ArbHcr)
011100040322     d ol_ArbGc1       s                   like(ArbGc1)
011200040322     d ol_ArbGc2       s                   like(ArbGc2)
011300040322     d ol_ArbTc1       s                   like(ArbTc1)
011400040322     d ol_ArbTc2       s                   like(ArbTc2)
011500040322     d ol_ArbFst       s                   like(ArbFst)
011600040322     d okarbg          s              1
011700040804     d KLIN            s                   like(arbLNP)
011800040804     D* Schiera p.o. da trasferire
011900040804     D  SKPO           s              3    dim(500)
012000950622     D*
012100000000     D KPJBA         E DS
012200040805     d fnevbs        e ds                  extname(fnevb00f) prefix(s)
012300040805     d fnevb         e ds                  extname(fnevb00f)
012400040805     d fnevb£        e ds                  extname(fnevb00f) prefix(£)
012500070430     d fnevb2s       e ds                  extname(fnevb20f) prefix(s)
012600070430     d fnevb2        e ds                  extname(fnevb20f)
012700070430     d fnevb2£       e ds                  extname(fnevb20f) prefix(£)
012706170210
012800040804     IFNARBD00      61
012900040804     IFNARBK00      62
013000040804     IFIARBT00      63
013100040804     IFNARBG00      65
013200040804     IFNARBM00      66
013300040804     IFNARBV00      67
013400040804     IFNARBP00      64
013500131011     IFNARBN00      68
013600980709     I*
013700950621     C*****************************************************************
013800000000     C     *ENTRY        PLIST
013900000000     C                   PARM                    KPJBA
014000040804     C                   PARM                    param          1500
014100040804     c
014200040804     c     ' ':'0'       xlate     param         param
014300040804     c
014400040804     c                   movea     param         skpo
014500950622     C*---------------------------------------------------------------*
014501170210     C     kblp          KLIST
014502170210     C                   KFLD                    BLPAAS
014503170210     C                   KFLD                    BLPLNP
014504170210     C                   KFLD                    BLPNRS
014505170210     C                   KFLD                    BLPNSP
015100991125     C*---------------------------------------------------------------*
015200060224     C     KAR4          KLIST
015300060224     C                   KFLD                    AR4AAS
015400060224     C                   KFLD                    AR4LNP
015500060224     C                   KFLD                    AR4NRS
015600060224     C                   KFLD                    AR4NSP
015700060224     C                   KFLD                    AR4TRC
015800060224     C     KAR6          KLIST
015900060224     C                   KFLD                    AR6AAS
016000060224     C                   KFLD                    AR6LNP
016100060224     C                   KFLD                    AR6NRS
016200060224     C                   KFLD                    AR6NSP
016300060224     C                   KFLD                    AR6TRC
016400040804     C     KAR7          KLIST
016500040804     C                   KFLD                    ar7AAS
016600040804     C                   KFLD                    ar7LNP
016700040804     C                   KFLD                    ar7NRS
016800040804     C                   KFLD                    ar7NSP
016900040804     C                   KFLD                    AR7TRC
017000080228     C                   KFLD                    AR7svn
017100080228     C     KAR8          KLIST
017200080228     C                   KFLD                    ar8AAS
017300080228     C                   KFLD                    ar8LNP
017400080228     C                   KFLD                    ar8NRS
017500080228     C                   KFLD                    ar8NSP
017600080228     C                   KFLD                    AR8TMD
017700080228     C                   KFLD                    AR8PRR
017800051014     C     kARS2         KLIST
017900051014     C                   KFLD                    arsfls
018000051014     C                   KFLD                    arslna
018100051014     C                   KFLD                    arsnrs
018200051014     C                   KFLD                    arsnsc
018300051014     C                   KFLD                    arstrc
018400900526     C*---------------------------------------------------------------*
018500980407     C     KARS          KLIST
018600980407     C                   KFLD                    ARTFLS
018700980407     C                   KFLD                    ARTLNA
018800980407     C                   KFLD                    ARTNRS
018900980407     C                   KFLD                    ARTNSC
019000980407     C*---------------------------------------------------------------*
019100980407     C     KARBF         KLIST
019200980407     C                   KFLD                    ARBAAS
019300980407     C                   KFLD                    ARBLNP
019400980407     C                   KFLD                    ARBNRS
019500980407     C                   KFLD                    ARBNSP
019600980407     C                   KFLD                    ARBDTV
019700980407     C                   KFLD                    ARBORV
019800991124     C     KARBU         KLIST
019900991124     C                   KFLD                    ARBAAS
020000991124     C                   KFLD                    ARBLNP
020100991124     C                   KFLD                    ARBNRS
020200991124     C                   KFLD                    ARBNSP
020300991124     C                   KFLD                    ARBTRC
020400991124     C                   KFLD                    ARBDTV
020500991124     C                   KFLD                    ARBORV
020600030122     c     K08AR501      klist
020700030122     c                   kfld                    AR5aas
020800030122     c                   kfld                    AR5lnp
020900030122     c                   kfld                    AR5nrs
021000030122     c                   kfld                    AR5nsp
021100030122     c                   kfld                    AR5trd
021200030122     c                   kfld                    AR5dac
021300030122     c                   kfld                    AR5orc
021400030122     c                   kfld                    AR5prg
021500040805     C     KEVB          KLIST
021600040805     C                   KFLD                    evbAAS
021700040805     C                   KFLD                    evbLNP
021800040805     C                   KFLD                    evbNRS
021900040805     C                   KFLD                    evbNSP
022000040805     C                   KFLD                    evbdev
022100040805     C                   KFLD                    evbhev
022200040805     C                   KFLD                    evbdtv
022300040805     C                   KFLD                    evborv
022400070430     C     KEVB2         KLIST
022500070430     C                   KFLD                    evb2AAS
022600070430     C                   KFLD                    evb2LNP
022700070430     C                   KFLD                    evb2NRS
022800070430     C                   KFLD                    evb2NSP
022900070430     C                   KFLD                    evb2DTV
023000070430     C                   KFLD                    evb2ORV
023100070430     C                   KFLD                    evb2CEV
023200040805     C     Klbl          KLIST
023300040805     C                   KFLD                    lblAAp
023400040805     C                   KFLD                    lbllpp
023500040805     C                   KFLD                    lblNRp
023600040805     C                   KFLD                    lblNSP
023700040805     C                   KFLD                    lblAAn
023800040805     C                   KFLD                    lbllpn
023900040805     C                   KFLD                    lblNRn
024000040805     C                   KFLD                    lblNSn
024100060208     C     Klblo         KLIST
024200060208     C                   KFLD                    lblAAo
024300060208     C                   KFLD                    lbllpo
024400060208     C                   KFLD                    lblNRo
024500060208     C                   KFLD                    lblNSo
024600060208     C     Klblp         KLIST
024700060208     C                   KFLD                    lblAAp
024800060208     C                   KFLD                    lbllpp
024900060208     C                   KFLD                    lblNRp
025000060208     C                   KFLD                    lblNSP
025100060208     C     Klbln         KLIST
025200060208     C                   KFLD                    lblAAn
025300060208     C                   KFLD                    lbllpn
025400060208     C                   KFLD                    lblNRn
025500060208     C                   KFLD                    lblNSn
025600980407     C*---------------------------------------------------------------*
025700040804     C*
025800040804     C* AZZERO CONTATORI
025900040804     C                   MOVE      *ZERO         RECARB            7 0
025901170210     C                   MOVE      *ZERO         RECblp            7 0
025902170215     C                   MOVE      *ZERO         RECblt            7 0
026000040804     C                   MOVE      *ZERO         RECAR4            7 0
026100040804     C                   MOVE      *ZERO         RecAR5            7 0
026101180205     C                   MOVE      *ZERO         Recbld            7 0
026102180205     C                   MOVE      *ZERO         Recpnd            7 0
026200040804     C                   MOVE      *ZERO         RECAR6            7 0
026300040804     C                   MOVE      *ZERO         RECAR7            7 0
026400080228     C                   MOVE      *ZERO         RECAR8            7 0
026500040804     C                   MOVE      *ZERO         RECAR9            7 0
026600040804     C                   MOVE      *ZERO         RECART            7 0
026601170210     C                   MOVE      *ZERO         RECARTi           7 0
026700040804     C                   MOVE      *ZERO         RECARS            7 0
026800040804     C                   MOVE      *ZERO         RECEVS            7 0
026900040804     C                   MOVE      *ZERO         RECVD             7 0
027000040804     C                   MOVE      *ZERO         RECVK             7 0
027100040804     C                   MOVE      *ZERO         RECVT             7 0
027200040804     C                   MOVE      *ZERO         RECVG             7 0
027300040804     C                   MOVE      *ZERO         RECVM             7 0
027400040804     C                   MOVE      *ZERO         RECVV             7 0
027500040804     C                   MOVE      *ZERO         RECVP             7 0
027600131011     C                   MOVE      *ZERO         RECVN             7 0
027700040804     C                   MOVE      *ZERO         RECVU             7 0
027800040804     C                   MOVE      *ZERO         RECARI            7 0
027900061114     C                   MOVE      *ZERO         RECARG            7 0
028000070424     C                   MOVE      *ZERO         RECARP            7 0
028100070424     C                   MOVE      *ZERO         RECARN            7 0
028200040915     C                   MOVE      *ZERO         RECEDR            7 0
028300060224     C                   MOVE      *ZERO         RECdp5            7 0
028400950622     C*
028401170202     c                   z-add     1             x                 4 0
028402170207     c* Bolle Partenza per lnp
028403170202     C* LEGGO i p.o. passati
028404170202    1c                   dow       skpo(x)>*zeros
028405170202     c                   movel     skpo(X)       klin
028406170202     C     KLIN          SETLL     FNblp37l
028407170202     C     KLIN          READE     FNblp37l                               30
028408170202     C*
028409170202    2C     *IN30         DOWEQ     *OFF
028410170202     C*
028411170202     C** ELABORAZIONE RECORDS RICEVUTI DELLE BOLLE
028412170202     C                   EXSR      AGGBOL
028413170202     C*
028414170202     C     KLIN          READE     FNblp37l                               30
028415170202    2C                   ENDDO
028416170202     C                   ADD       1             X
028417170202     C                   ENDDO
028418170207
028419170207     c                   z-add     1             x                 4 0
028421170207     c* Bolle Partenza per LNA scartando quelle con lnp già elaborate nel giro precedente
028422170207     C* LEGGO i p.o. passati
028423170207    1c                   dow       skpo(x)>*zeros
028424170207     c                   movel     skpo(X)       klin
028425170207     C     KLIN          SETLL     FNblp26l
028426170207     C     KLIN          READE     FNblp26l                               30
028427170207     C*
028428170207    2C     *IN30         DOWEQ     *OFF
028429170210     c                   move      blplnp        w003a
028430170210     c                   if        %lookup(w003a:skpo)=0
028431170207     C*
028432170207     C** ELABORAZIONE RECORDS RICEVUTI DELLE BOLLE
028433170207     C                   EXSR      AGGBOL
028434170207     c                   endif
028435170207     C*
028436170207     C     KLIN          READE     FNblp26l                               30
028437170207    2C                   ENDDO
028438170207     C                   ADD       1             X
028439170207     C                   ENDDO
030100060208     c
030200060208     c* Terminata la scrittura del file, leggo i legamo bolla
030300060208     c*  e carico le bolle legate non scaricate
030400060208     c     *loval        setll     fnlblnew
030500060208     c                   read      fnlblnew
030600060208     c                   dow       not %eof(fnlblnew)
030700060208     c* Bolla originale
030800060208     c                   movel     lbllao        w003a             3
030900060208     c     w003a         lookup    skpo                                   36
031000060208     c                   if        not *in36
031100170207     C     klblo         chain     FNblp01l                           30
031200060208     C  n30              EXSR      AGGBOL
031300060208     c                   endif
031400060208     c
031500060208     c* Bolla mamma se <> dall'originale
031600060208     c                   if        lblaao<>lblaap or lbllpo<>lbllpp or
031700060208     c                             lblnro<>lblnrp or lblnso<>lblnsp
031800060208     c                   movel     lbllap        w003a             3
031900060208     c     w003a         lookup    skpo                                   36
032000060208     c                   if        not *in36
032100170207     C     klblp         chain     FNblp01l                           30
032200060208     C  n30              EXSR      AGGBOL
032300060208     c                   endif
032400060208     c                   endif
032500060208     c* Bolla figlia
032600060208     c                   movel     lbllan        w003a             3
032700060208     c     w003a         lookup    skpo                                   36
032800060208     c                   if        not *in36
032900170207     C     klbln         chain     FNblp01l                           30
033000060208     C  n30              EXSR      AGGBOL
033100060208     c                   endif
033200060208     c*
033300060208     c                   read      fnlblnew
033400060208     c                   enddo
033500060208     C*
033600040804     c
033700040804     C                   EXCEPT    TESTA
033800040804     C                   EXCEPT    totali
033900000000     C                   SETON                                        LR
034000950622     C**-------------------------------------------------------------**
034100950622     C**  AGGIORNAMENTO FILES BOLLE                                  **
034200950622     C**-------------------------------------------------------------**
034300950622     C     AGGBOL        BEGSR
034400950622     C*
034600950622     C*
034700170202     C     Kblp          SETLL     FNBLPn                                 31
034701170210     C* se non trovata
035300950630    0C     *IN31         IFEQ      *OFF
035600950622     C*
035700060224     C****  FiAR400F  ****
035800170210     C     Kblp          SETLL     FiAR401l
035900170210     C     Kblp          READE     FiAR401l                               31
036000950622     C*
036100020703    1C     *IN31         DOWEQ     *OFF
036200060224     c     Kar4          chain     FIAR4new
036300060224    4c                   if        Not %found(FIAR4NEW)
036400950622     C                   ADD       1             RECAR4
036500060224     C                   WRITE     FiAR4n
036600060224     c                   endif
036700040804     C*    0
036800170210     C     Kblp          READE     FiAR401l                               31
036900020703    1C                   ENDDO
036901180205     C****  FIPND00F  ****
036902180205     C     KBLP          SETLL     FIPND01l
036903180205     C     KBLP          READE     FIPND01l                               31
036904180205     C*
036905180205    2C     *IN31         DOWEQ     *OFF
036906180205     c                   add       1             RECpnd
036907180205     C                   WRITE(e)  FIPNDN
036908180205     C*
036909180205     C     KBLP          READE     FIPND01l                               31
036910180205    2C                   ENDDO
036911180205     C****  FNBLD00F  ****
036912180205     C     KBLP          SETLL     FNBLD01l
036913180205     C     KBLP          READE     FNBLD01l                               31
036914180205     C*
036915180205    2C     *IN31         DOWEQ     *OFF
036916180205     c                   add       1             RECbld
036917180205     C                   WRITE(e)  FNBLDN
036918180205     C*
036919180205     C     KBLP          READE     FNBLD01l                               31
036920180205    2C                   ENDDO
037000040323     c
037100040323     C****  FNEVS00F con spedizione senza segnacollo *****
037200170210     C     Kblp          SETLL     FNEVS01l
037300170210     C     Kblp          READE     FNEVS01l                               31
037400040323     C*
037500040323    1C     *IN31         DOWEQ     *OFF
037600040323     c                   if        evsnsc=0
037700040323     C                   ADD       1             RECevs
037800170215     C                   WRITE(e)  FNevsN
037900040323     c                   endif
038000040323     C*
038100170210     C     Kblp          READE     FNEVS01l                               31
038200040323    1C                   ENDDO
038300030121      *
038400030121      ****  FIAR500F  ****
038500170210     c     Kblp          setll     FIAR501l
038600170210     c     Kblp          reade     FIAR501l
038700040804    2c                   dow       Not %eof(FIAR501l)
038800060224     c     K08AR501      chain     FIAR5new
038900040804    4c                   if        Not %found(FIAR5NEW)
039000040804     c                   add       1             RecAR5
039100040805     c                   WRITE     FIAR5n                               99
039200030122   -4c                   endif
039300040804     c
039400170210     c     Kblp          reade     FIAR501l
039500030121   -2c                   enddo
039600950622     C*
039700040804     C****  FIAR600F  ****
039800170210     C     Kblp          SETLL     FIAR601l
039900170210     C     Kblp          READE     FIAR601l                               31
040000040804    2C     *IN31         DOWEQ     *OFF
040100040804     c     KAR6          chain     FIAR6N
040200040804    3c                   if        Not %found(FIAR6NEW)
040300040805     C                   ADD       1             RECAR6
040400040804     c                   WRITE     FIAR6N                               99
040500040804   -3c                   endif
040600170210     C     Kblp          READE     FIAR601l                               31
040700040804     c                   enddo
040800040804     C****  FIAR700F  ****
040900170210     C     Kblp          SETLL     FIAR701l
041000170210     C     Kblp          READE     FIAR701l                               31
041100040804    2C     *IN31         DOWEQ     *OFF
041200040804     c     KAR7          chain     FIAR7N
041300040804    3c                   if        Not %found(FIAR7NEW)
041400040805     C                   ADD       1             RECAR7
041500040804     c                   WRITE     FIAR7N                               99
041600040804   -3c                   endif
041700170210     C     Kblp          READE     FIAR701l                               31
041800040804     c                   enddo
041900080228     C****  FIAR800F  ****
042000170210     C     Kblp          SETLL     FIAR801l
042100170210     C     Kblp          READE     FIAR801l                               31
042200080228    2C     *IN31         DOWEQ     *OFF
042300080228     c     KAR8          chain     FIAR8N
042400080228    3c                   if        Not %found(FIAR8NEW)
042500080228     C                   ADD       1             RECAR8
042600080228     c                   WRITE     FIAR8N                               99
042700080228   -3c                   endif
042800170210     C     Kblp          READE     FIAR801l                               31
042900080228     c                   enddo
043000950622     C*
043100051118     C****  FiAR900F  ****
043200170210     C     Kblp          CHAIN     FiAR901l                           31
043300950622    2C     *IN31         IFEQ      *OFF
043400170210     c     Kblp          chain     FIAR9N
043500051118    3c                   if        Not %found(FIAR9NEW)
043600950622     C                   ADD       1             RECAR9
043700051118     C                   WRITE     FiAR9N
043800950622    2C                   ENDIF
043900051118    2C                   ENDIF
044000080228     C*
044001170215     C****  FNBLT00F  ****
044002170215     C     Kblp          SETLL     FNblt01l
044003170215     C     Kblp          READE     FNblt01l                               31
044004170215     C*
044005170215    2C     *IN31         DOWEQ     *OFF
044006170215     C                   ADD       1             RECblt
044007170215     C                   WRITE(e)  FNbltN
044019170215     c
044028170215     C*
044029170215     C     Kblp          READE     FNblt01l                               31
044030170215    2C                   ENDDO
044100950622     C****  FNART00F  ****
044200170210     C     Kblp          SETLL     FNART01l
044300170210     C     Kblp          READE     FNART01l                               31
044400950622     C*
044500950622    2C     *IN31         DOWEQ     *OFF
044600950622     C                   ADD       1             RECART
044700170215     C                   WRITE(e)  FNARTN
044800051014     C****  FIARS00F  ****
044900051014     C     KARS          SETLL     FiARS01l
045000051014     C     KARS          READE     FiARS01l                               32
045100980407    4C     *IN32         DOWEQ     *OFF
045200051014     c     KARs2         chain     FIARsN
045300051014    3c                   if        Not %found(FIARSNEW)
045400051014     C                   ADD       1             RECARS
045500051014     C                   WRITE     FiARSN
045600051014     c                   endif
045700051014     C     KARS          READE     FiARS01l                               32
045800980407    4C                   ENDDO
045900060224     c
046000980407     C****  FNEVS00F  ****
046100040804     C     KARS          SETLL     FNEVS03l
046200040804     C     KARS          READE     FNEVS03l                               32
046300980407    4C     *IN32         DOWEQ     *OFF
046400980407     C                   ADD       1             RECEVS
046500170215     C                   WRITE(e)  FNEVSN
046600040804     C     KARS          READE     FNEVS03l                               32
046700980407    4C                   ENDDO
046800950622     C*
046900170210     C     Kblp          READE     FNART01l                               31
047000950622    2C                   ENDDO
047001170209     C****  FIART00F  ****
047002170210     C     Kblp          SETLL     FiARt01l
047003170210     C     Kblp          READE     FiARt01l                               32
047004170209    4C     *IN32         DOWEQ     *OFF
047007170209     C                   ADD       1             RECARTI
047008170215     C                   WRITE(e)  FiARTN
047010170210     C     Kblp          READE     FiART01l                               32
047011170209    4C                   ENDDO
047100991223     C****  FIARI00F  ****
047200170210     C     KBLP          SETLL     FIARI01l
047300170210     C     KBLP          READE     FIARI01l                               31
047400991223     C*
047500991223    2C     *IN31         DOWEQ     *OFF
047600991223     C                   ADD       1             RECARI
047700170215     C                   WRITE(e)  FIARIn
047800991223     C*
047900170210     C     KBLP          READE     FIARI01l                               31
048000991223    2C                   ENDDO
048100060224     C*
048200061114     C****  FIARG00F  ****
048300170210     C     KBLP          SETLL     FIARG01l
048400170210     C     KBLP          READE     FIARG01l                               31
048500061114     C*
048600061114    2C     *IN31         DOWEQ     *OFF
048700061114     C                   ADD       1             RECARG
048800170215     C                   WRITE(e)  FIARGn
048900061114     C*
049000170210     C     KBLP          READE     FIARG01l                               31
049100061114    2C                   ENDDO
049200070424     C****  FiARP00F  ****
049300170210     C     KBLP          CHAIN     FiARP01l                           31
049400070424    2C     *IN31         IFEQ      *OFF
049500170210     c     KBLP          chain     FIARPN
049600070424    3c                   if        Not %found(FIARPNEW)
049700070424     C                   ADD       1             RECARP
049800070424     C                   WRITE     FiARPN
049900070424    2C                   ENDIF
050000070424    2C                   ENDIF
050100070424     C*
050200070424     C****  FIARN00F  ****
050300170210     C     KBLP          SETLL     FIARN01l
050400170210     C     KBLP          READE     FIARN01l                               31
050500070424     C*
050600070424    2C     *IN31         DOWEQ     *OFF
050700070424     C                   ADD       1             RECARN
050800170215     C                   WRITE(e)  FIARNn
050900070424     C*
051000170210     C     KBLP          READE     FIARN01l                               31
051100070424    2C                   ENDDO
051200060224     C*
051300060224     C****  FIdp500F  ****
051400170210     C     KBLP          SETLL     FIdp501r
051500170210     C     KBLP          READE     FIdp501r                               31
051600060224    2C     *IN31         DOWEQ     *OFF
051700170210     c     KBLP          chain     FIdp5N
051800060224    3c                   if        Not %found(FIdp5NEW)
051900060224     C                   ADD       1             RECdp5
052000060224     c                   WRITE     FIdp5N                               99
052100060224     c
052200170210     C     KBLP          READE     FIdp501r                               31
052300060224     c                   else
052400060224     c                   seton                                        31
052500060224   -3c                   endif
052600060224     c                   enddo
052700040805     C*
052800040805     C****  FNEVB00F  ****
053400170210     C     KBLP          SETLL     FNEVB01l
053500170210     C     KBLP          READE     FNEVB01l                               31
053600040805    2C     *IN31         DOWEQ     *OFF
053700040805    c
053800170210     c
053801170213     c                   eval      fnevbs=fnevb
053900040805     c                   clear                   nowrite           1
054000040805     c     Kevb          setll     FnevbN
054100040805     c     Kevb          reade     FnevbN
054200040805    3c                   dow       Not %eof(FnevbNEW)
054300040805    4c                   if        fnevbs = fnevb£
054400040805     c                   eval      nowrite='N'
054500040805    4c                   endif
054600040805     c     Kevb          reade     FnevbN
054700040805    3c                   enddo
054800040805    c
054900040805    3c                   if        nowrite=' '
055000040805     c                   eval      fnevb£=fnevbs
055100040805     c                   WRITE     FnevbN                               99
055200040805    3c                   endif
055300040805     c
055400170210     C     KBLP          READE     Fnevb01l                               31
055500040805    2c                   enddo
055600040805     C*
055700070430     C****  FNEVB20F  ****
055800170210     C     KBLP          SETLL     FNEVB22l
055900170210     C     KBLP          READE     FNEVB22l                               31
056000070430    2C     *IN31         DOWEQ     *OFF
056100070430    c
056200070430     c                   eval      fnevb2s=fnevb2
056300070430     c                   clear                   nowrite           1
056400070430     c     Kevb2         setll     Fnevb2N
056500070430     c     Kevb2         reade     Fnevb2N
056600070430    3c                   dow       Not %eof(Fnevb2NW)
056700070430    4c                   if        fnevb2s = fnevb2£
056800070430     c                   eval      nowrite='N'
056900070430    4c                   endif
057000070430     c     Kevb2         reade     Fnevb2N
057100070430    3c                   enddo
057200070430    c
057300070430    3c                   if        nowrite=' '
057400070430     c                   eval      fnevb2£=fnevb2s
057500070430     c                   WRITE     Fnevb2N                              99
057600070430    3c                   endif
057700070430     c
057800170210     C     KBLP          READE     Fnevb22l                               31
057900070430    2c                   enddo
058000070430     C*
058100040805     C****  FNLBL00F  ****
058200170210     C     KBLP          SETLL     FNlbl01l
058300170210     C     KBLP          reade     FNlbl01l                               31
058400040805    2C     *IN31         DOWEQ     *OFF
058500040805     c
058600040805     c* se trovata verifico che non esiste già il legame bolla
058700040805     c*  come mamma+figlia key univoca
058800040805     c     klbl          setll     fnlblnew                               35
058900040805     c                   if        not *in35
059000040805     c                   write     fnlbln
059100040805     c                   endif
059200040805     c
059300170210     C     Kblp          reade     FNlbl01l                               31
059400040805     c                   enddo
059500040805     c
059600170210     C     Kblp          SETLL     FNlbl02l
059700170210     C     Kblp          reade     FNlbl02l                               31
059800040805    2C     *IN31         DOWEQ     *OFF
059900040805     c
060000040805     c* se trovata verifico che non esiste già il legame bolla
060100040805     c*  come mamma+figlia key univoca
060200040805     c     klbl          setll     fnlblnew                               35
060300040805     c                   if        not *in35
060400040805     c                   write     fnlbln
060500040805     c                   endif
060600040805     c
060700170210     C     Kblp          reade     FNlbl02l                               31
060800040805     c                   enddo
061000040915     C* EDRDE
061100170210     C     Kblp          SETLL     EDRDE01l
061200170210     C     Kblp          READE     EDRDE01l                               31
061300040915     C*
061400040915    2C     *IN31         DOWEQ     *OFF
061500040915     C                   ADD       1             RECEDR
061600170215     C                   WRITE     RDENEW                               99
061700170210     C     Kblp          READE     EDRDE01l                               31
061800040915     C                   ENDDO
061900040805     C*
062000950622     C*
062100020703     c                   exsr      aggvar
062101170208
062102170208     C*****
062103170208     C***  B O L L A
062104170208     C*****
062107170210     c     kblp          chain     fnarb01l
062108170208     c                   if        %found(fnarb01l)
062109170208     c                   add       1             recarb
062110170215     c                   write(e)  fnarbn
062111170208     c                   endif
062112170210
062113170210     c                   add       1             recblp
062114170210     c                   write     fnblpn
062115170208
062200950630    0C                   ENDIF
062300950630     C*
062400900530     C                   ENDSR
062500950622     C**-------------------------------------------------------------**
062600950622     C**  AGGIORNAMENTO FILES VARIAZIONI BOLLE                       **
062700950622     C**-------------------------------------------------------------**
062800020703     C     AGGVAR        BEGSR
062900020703     C                   SETOFF                                       616263
063000020703     C                   SETOFF                                       646566
063100131011     C                   SETOFF                                       6768
063200170210     c     kblp          setll     fIarbf2c
063300170210     c     kblp          reade     fIarbf2c                               31
063400020703     C                   DOW       NOT *IN31
063500020703     C*
063600020703     C****  FNARBD0F  ****
063700020703     C** VERIFICO SE RECORD GIA' ESISTENTE IN ARCHIVIO
063800020703     c                   if        *in61
063900020703     C     KARBF         CHAIN     FNARBD1                            35
064000020703    2C     *IN35         IFEQ      *ON
064100020703     C* SCRIVO IL RECORD
064200020703     C                   ADD       1             RECVD
064300040804     C                   WRITE     FNARBDn
064400020703    2c                   endif
064500020703    1c                   endif
064600020703     C*
064700020703     C****  FNARBK0F  ****
064800020703     C** VERIFICO SE RECORD GIA' ESISTENTE IN ARCHIVIO
064900020703    1c                   if        *in62
065000020703     C     KARBF         CHAIN     FNARBK1                            35
065100020703    2C     *IN35         IFEQ      *ON
065200020703     C* SCRIVO IL RECORD
065300020703     C                   ADD       1             RECVK
065400040804     C                   WRITE     FNARBKn
065500020703    2C                   END
065600020703    1C                   END
065700020703     C*
065800020703     C****  FIARBT0F  ****
065900020703     C** VERIFICO SE RECORD GIA' ESISTENTE IN ARCHIVIO
066000020703    1c                   if        *in63
066100020703     C     KARBF         CHAIN     FIARBT1                            35
066200020703    2C     *IN35         IFEQ      *ON
066300020703     C* SCRIVO IL RECORD
066400020703     C                   ADD       1             RECVT
066500040804     C                   WRITE     FIARBTn
066600020703     C** GUARDO SE ESISTE L'ESTENSIONE FIARBT
066700020703     C****  FIARBU0F  ****
066800040804     C     KARBU         SETLL     FIARBU1l
066900040804     C     KARBU         READE     FIARBU1l                               32
067000020703    3C     *IN32         DOWEQ     *OFF
067100040804     C                   WRITE     FIARBUn
067200020703     C                   ADD       1             RECVU
067300040804     C     KARBU         READE     FIARBU1l                               32
067400020703    3C                   ENDDO
067500020703    2C                   END
067600020703    1C                   END
067700020703     C*
067800020703     C****  FNARBG0F  ****
067900020703     C** VERIFICO SE RECORD GIA' ESISTENTE IN ARCHIVIO
068000020703    1c                   if        *in65
068100040322     c                   Eval      okarbg = '1'
068200040322      * salvo i campi del record letto
068300040322     c                   Eval      ol_ArbCbo = ArbCbo
068400040322     c                   Eval      ol_ArbCvb = ArbCvb
068500040322     c                   Eval      ol_ArbTcr = ArbTcr
068600040322     c                   Eval      ol_ArbDcr = ArbDcr
068700040322     c                   Eval      ol_ArbHcr = ArbHcr
068800040322     c                   Eval      ol_ArbGc1 = ArbGc1
068900040322     c                   Eval      ol_ArbGc2 = ArbGc2
069000040322     c                   Eval      ol_ArbTc1 = ArbTc1
069100040322     c                   Eval      ol_ArbTc2 = ArbTc2
069200040322     c                   Eval      ol_ArbFst = ArbFst
069300040322      * controllo se c'è già una variazione uguale a quella che sto leggendo
069400040322     c     kArbf         Setll     Fnarbg1
069500040322do  2c                   Do        *Hival
069600040322     c     kArbf         Reade     Fnarbg1
069700040322     c                   If        %Eof
069800040322     c                   Leave
069900040322     c                   EndIf
070000040322      * per dire che non c'è già una variazione uguale devo controllare la causale
070100040322      * caso trovato delle nuove causali 'CA' create in automatico da LR48
070200040322     c                   If        Arbcvb = ol_ArbCvb
070300040322     c                   Eval      okarbg = '0'
070400040322     c                   EndIf
070500040322    2c                   EndDo
070600020703     C* SCRIVO IL RECORD
070700040322if  2c                   If        okarbg = '1'
070800040322     c                   Eval      ArbCbo = ol_ArbCbo
070900040322     c                   Eval      ArbCvb = ol_ArbCvb
071000040322     c                   Eval      ArbTcr = ol_ArbTcr
071100040322     c                   Eval      ArbDcr = ol_ArbDcr
071200040322     c                   Eval      ArbHcr = ol_ArbHcr
071300040322     c                   Eval      ArbGc1 = ol_ArbGc1
071400040322     c                   Eval      ArbGc2 = ol_ArbGc2
071500040322     c                   Eval      ArbTc1 = ol_ArbTc1
071600040322     c                   Eval      ArbTc2 = ol_ArbTc2
071700040322     c                   Eval      ArbFst = ol_ArbFst
071800020703     C                   ADD       1             RECVG
071900040804     C                   WRITE     FNARBGn
072000020703    2C                   END
072100020703    1C                   END
072200020703     C*
072300020703     C****  FNARBM0F  ****
072400020703     C** VERIFICO SE RECORD GIA' ESISTENTE IN ARCHIVIO
072500020703    1c                   if        *in66
072600020703     C     KARBF         CHAIN     FNARBM1                            35
072700020703    2C     *IN35         IFEQ      *ON
072800020703     C* SCRIVO IL RECORD
072900020703     C                   ADD       1             RECVM
073000040804     C                   WRITE     FNARBMn
073100020703    2C                   END
073200020703    1C                   END
073300020703     C*
073400020703     C****  FNARBV0F  ****
073500020703     C** VERIFICO SE RECORD GIA' ESISTENTE IN ARCHIVIO
073600020703    1c                   if        *in67
073700031124     c                   Eval      okvol = '1'
073800031124      * salvo i campi causale variazione - volume e flag volume
073900031124     c                   Eval      ol_ArbCbo = ArbCbo
074000031124     c                   Eval      ol_ArbCvb = ArbCvb
074100031124     c                   Eval      ol_ArbVlc = ArbVlc
074200031124     c                   Eval      ol_ArbFvc = ArbFvc
074300031124     c                   Eval      ol_ArbNcr = ArbNcr
074400031124      * controllo se c'è già una variazione di volume uguale a quella che sto leggendo
074500031124     c     kArbf         Setll     Fnarbv1
074600031124do  2c                   Do        *Hival
074700031124     c     kArbf         Reade     Fnarbv1
074800031124     c                   If        %Eof
074900031124     c                   Leave
075000031124     c                   EndIf
075100031124      * per dire che non c'è già una variazione uguale devo controllare anche i campi salvati
075200031124     c                   If        Arbcvb = ol_ArbCvb and
075300031124     c                             ArbVlc = ol_ArbVlc and
075400031124     c                             ArbFvc = ol_ArbFvc
075500031124     c                   Eval      okvol = '0'
075600031124     c                   EndIf
075700031124    2c                   EndDo
075800020703     C* SCRIVO IL RECORD
075900031124if  2c                   If        okvol = '1'
076000031124     c                   Eval      ArbCbo = ol_ArbCbo
076100031124     c                   Eval      ArbCvb = ol_ArbCvb
076200031124     c                   Eval      ArbVlc = ol_ArbVlc
076300031124     c                   Eval      ArbFvc = ol_ArbFvc
076400031124     c                   Eval      ArbNcr = ol_ArbNcr
076500020703     C                   ADD       1             RECVV
076600040804     C                   WRITE     FNARBVn
076700020703    2C                   END
076800020703    1C                   END
076900020703     C*
077000020703     C****  FNARBP0F  ****
077100020703     C** VERIFICO SE RECORD GIA' ESISTENTE IN ARCHIVIO
077200020703    1c                   if        *in64
077300020703     C     KARBF         CHAIN     FNARBP1                            35
077400020703    2C     *IN35         IFEQ      *ON
077500020703     C* SCRIVO IL RECORD
077600020703     C                   ADD       1             RECVP
077700040804     C                   WRITE     FNARBPn
077800020703    2C                   END
077900020703    1C                   END
078000131011
078100131011     c                   if        *in68
078200131011     C     KARBF         CHAIN     FNARBN1                            35
078300131011    2C     *IN35         IFEQ      *ON
078400131011     C* SCRIVO IL RECORD
078500131011     C                   ADD       1             RECVn
078600131011     C                   WRITE     FNARBNN
078700131011    2c                   endif
078800131011    1c                   endif
078900020703     C***
079000020703     C                   SETOFF                                       616263
079100020703     C                   SETOFF                                       646566
079200131011     C                   SETOFF                                       6768
079300170210     c     kblp          reade     fIarbf2C                               31
079400020703     C                   ENDDO
079500020703     C*
079600020703     C                   ENDSR
079700950622     C*---------------------------------------------------------------*
079800901006     OSYSPRT    E            TESTA          2 02
079900180205     O                                         +  5 'TOTALE BOLLE       '
080000180205     O                                         +  5 'FITS42R'
080100901006     O          E            TESTA          2
080200020703     O          E            totali      2
080300020703     O                                         +  0 'FNARB'
080400020703     O                       recarb        2   +  1
080500020703     O                                         +  2 'FNAR4'
080600020703     O                       recar4        2   +  1
080700030121     O                                         +  2 'FIAR5'
080800030121     O                       recAR5        2   +  1
080900020703     O                                         +  2 'FNAR6'
081000020703     O                       recar6        2   +  1
081100020703     O                                         +  2 'FNAR7'
081200020703     O                       recar7        2   +  1
081300080228     O                                         +  2 'FNAR8'
081400080228     O                       recar8        2   +  1
081500051118     O                                         +  2 'FIAR9'
081600020703     O                       recar9        2   +  1
081700020703     O          E            totali      2
081800020703     O                                         +  2 'FNARS'
081900020703     O                       recarS        2   +  1
082000020703     O                                         +  2 'FNEVS'
082100020703     O                       recEVS        2   +  1
082200020703     O                                         +  2 'FIARI'
082300020703     O                       recARI        2   +  1
082400020703     O                                         +  2 'FNARBD'
082500020703     O                       recVD         2   +  1
082600020703     O                                         +  2 'FNARBK'
082700020703     O                       recVK         2   +  1
082800020703     O                                         +  2 'FIARBT'
082900020703     O                       recVT         2   +  1
083000020703     O                                         +  2 'FIARBU'
083100020703     O                       recVU         2   +  1
083200020703     O          E            totali      2
083300020703     O                                         +  2 'FNARBG'
083400020703     O                       recVG         2   +  1
083500020703     O                                         +  2 'FNARBM'
083600020703     O                       recVM         2   +  1
083700020703     O                                         +  2 'FNARBV'
083800020703     O                       recVV         2   +  1
083900020703     O                                         +  2 'FNARBP'
084000080228     O                       recVP         2   +  1
084100131011     O                                         +  2 'FNARBN'
084200131011     O                       recVn         2   +  1
084300080228     O                                         +  2 'FNART'
084400080228     O                       recarT        2   +  1
084500040915     O                                         +  2 'EDRDE00F'
084600040915     O                       recEDR        2   +  1
084700060224     O          E            totali      2
084800060224     O                                         +  2 'FIDP5'
084900060224     O                       recdp5        2   +  1
085000061114     O                                         +  2 'FIARG'
085100061114     O                       recARG        2   +  1
085200070424     O                                         +  2 'FIARP'
085300070424     O                       recARP        2   +  1
085400070424     O                                         +  2 'FIARN'
085500070424     O                       recARN        2   +  1
085600170210     O                                         +  2 'FIART'
085700170210     O                       recARti       2   +  1
085800170210     O                                         +  2 'FNBLP'
085900170210     O                       recblp        2   +  1
086000170215     O                                         +  2 'FNBLT'
086100170215     O                       recblt        2   +  1
086200180205     O          E            totali      2
086300180205     O                                         +  2 'FIPND'
086400180205     O                       recpnd        2   +  1
086500180205     O                                         +  2 'FNBLD'
086600180205     O                       recbld        2   +  1
