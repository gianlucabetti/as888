000100911127     H DECEDIT('0,') DATEDIT(*DMY.)
000200941109      * FNLR55R *----------------------------------------------------*
000300911127      *-----------------*                                            *
000400030414      *     - STAMPA DISTINTE CONSEGNA  -                            *
000500911127      *--------------------------------------------------------------*
000600030723     FFNARB70L  uF   E           K DISK
000700990921     F                                     RENAME(FNARB000:FNARB30)
000800941109     FFNLBL01L  IF   E           K DISK
000900941109     FFNARB01L  IF   E           K DISK
001000990920     FFIAR601L  IF   E           K DISK
001100051115     FFiAR901L  IF   E           K DISK
001200990920     FFIARI01L  IF   E           K DISK
001300021204     Ffiapd01L  IF   E           K DISK
001400020916     FFNFVV09L  IF   E           K DISK
001500041013     FFiqdt01L  IF   E           K DISK
001600990921     FFNART01L  IF   E           K DISK
001700911128     FTABEL00F  IF   E           K DISK
001800990921     FTNTBE01L  IF   E           K DISK
001900000516     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002000060214     FFiAR401L  IF   E           K DISK
002100061120     FFiAR501l  IF   E           K DISK
002200000606     FFNEVB05L  IF   E           K DISK
002300010215      *--
002400010215     FFNORM06L  IF   E           K DISK
002500080401     FFidst09l  if   e           k disk    prefix(F_)
002600010215      *--
002700040928     FFNLRF7P   O    E             PRINTER OFLIND(*IN80)
002800040930     F                                     INFDS(STAMP)
002900000606     FSYSPRT    O    F  132        PRINTER INFDS(STAMP2)
003000000606     F                                     OFLIND(*INOG)
003100941109      *--------------------------------------------------------------*
003200041013     D STT             S             66    DIM(8) CTDATA PERRCD(1)
003300911127     D SCN             S              7  0 DIM(13)                              SEGNACOLLI
003400920909     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
003500961217     D STP             S              3  0 DIM(999)                             1° schiera Stop
003600941110     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
003700950124     D D3A             S             70    DIM(100)                             FLAG. TBLUNI  3A
003800000606     D C2Z             S              3    DIM(100)                             CHIAVI EVENTI
003900000606     D D2Z             S             16    DIM(100)                             DS EVENTI
004000941109     D*  DS x gestione contatore di righe
004100941109     D STAMP           DS
004200941109     D  LIN                  367    368B 0
004300000606     D STAMP2          DS
004400000606     D  LIN2                 367    368B 0
004500941109     D*
004600911127     D KPJBA         E DS
004700911127     D  LIBSYS                92    100
004800911127     D CNCR80        E DS
004900020503     d trul06ds      e ds
005000020503     d  lin£                   1     90  0 dim(30)
005100911127     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005200941109      *
005300941109     D WLBDA8          DS
005400941109     D  G02DAT                 1      8  0
005500941109     D  G02INV                 9     16  0
005600941109     D  G02ERR                17     17
005700941109     D  G02TGI                18     22  0
005800911127      *
005900990921     D YEURDS        E DS                  EXTNAME(YEURCODS)
006000000606     D DS2Z          E DS
006100990921     D DGED          E DS
006200941110     D DSCV          E DS
006300911204     D DS3A          E DS
006400050518     D*DS7AF         E DS
006500920528     D DS3I          E DS
006600941109     D FNLR55        E DS                  EXTNAME(FNLR55DS)
006700000516     D DSBL4A        E DS
006800000516     D OG143         E DS
006900080723     D dar5ban       E DS                  inz
007000080723     D dar5bnb       E DS                  inz
007100061120     D dar5gen       E DS
007200080401     D ddstflr       E DS
007300081117     D ds7r          E DS
007400080723     d nban            S              5  0
007500031118     D ARBCCAS         S                   LIKE(ARBCCA) INZ
007600040930     D stcndc          S                   LIKE(ARBndc) INZ
007700040930     D stcddc          S                   LIKE(ARBddc) INZ
007800040930     D stcpdr          S                   LIKE(Apdpdr) INZ
007900040930     D stdpdr          S                   LIKE(Apdrsc) INZ
008000070612     d wpes            s                   like(arbpkf)
008100070612     d wvol            s                   like(arbvlf)
008200080401     d kfindc          s                   like(f_dstnfv)
008300040930     D                SDS
008400040930     D  NOMPGM                 1     10
008500941228      *---------------------------------------------------------------*
008600941228     D CMAT            C                   CONST('MATTINO   ')
008700941228     D CPOM            C                   CONST('POMERIGGIO')
008800941109      *--------------------------------------------------------------*
008900000516     IAZORG01L  AA
009000000516     I                             P    4    5 0ORGFIL
009100000516     I                                  7    7  ORGFAG
009200000516     I                               3776 3800  ORGDE3
009300941109      *--------------------------------------------------------------*
009400941109      *  CICLO PRINCIPALE
009500941109      *--------------------------------------------------------------*
009600950531     C     *ENTRY        PLIST
009700950531     C                   PARM                    KPJBA
009800950531     C                   MOVEL     KPJBU         FNLR55
009900080723     c                   clear                   nban
010000950911     C     D55TPR        IFEQ      'U'
010100950911     C                   SETON                                        LR
010200030723     c                   return
010300030723     C                   End
010400040929     c* stampa "firma x ricevuta"
010500050204     C     D55FIR        COMP      'S'                                    02
010600050204     C* IMPOSTO L'INDICATORE PER STAMPARE IL TITOLO IN BASE AL PARAMETRO
010700050204     C* FIRMA SI/NO CHE MI FA CAPIRE SE SONO IN CONTROLLO DISTINTA DI USCITA
010800050204     C* O IN STAMPA DISTINTA
010900050204     C                   IF        *IN02
011000050204     C                   SETON                                        61
011100050204     C                   SETOFF                                       62
011200050204     C                   ELSE
011300050204     C                   SETOFF                                       61
011400050204     C                   SETON                                        62
011500050204     C                   END
011600030625     c* stampa solo i totali
011700030625     C     D55tot        COMP      'S'                                    04
011800040930     c                   eval      prtfgs = d55fil
011900040930     c* stampa TESTATA
012000040930     c                   if        d55nwp=' ' or (d55pag=0 and d55lin=0)
012100040930     C                   seton                                            80
012200040930     c                   end
012300040930     C* NUMERO PAGINA
012400040930     C                   EVAL      d59pag = D55PAG
012500040930     C* NUMERO RIGA
012600040930     C                   EVAL      LIN = D55LIN
012700040930     C                   IF        LIN > 58
012800040930     C                   SETON                                        80
012900040930     c                   else
013000040930     c                   if        lin > 0
013100040930     c                   write     tests3
013200040930     c  N04              write     tests4
013300081117     c   04              write     tests5
013400040930     c                   end
013500040930     C                   END
013600030723     C                   Z-ADD     0             WNSS              3 0
013700040929     c* se cambia il po di gestione rifaccio la DEFCAM
013800020919     c                   if        d55fil <> kfgs
013900020919     c                   clear                   wprima
014000020919     c                   end
014100950531     C* Prima volta: richiamo routine iniz. variabiali
014200950531     C     WPRIMA        IFEQ      ' '
014300950531     C                   EXSR      DEFCAM
014400950531     C                   MOVEL     'N'           WPRIMA            1
014500950531     C                   END
014600941109     C* Leggo in archivio fogli viaggio tutti quelli che richiedo
014700950102     C                   Z-ADD     D55NRD        FVVNFV
014800911127     C*
014900020916     C     KFVV          SETLL     FNFVV09L
0150000409291    C                   DO        *HIVAL
015100040929     C     kfvv1         READE     FNFVV09L
015200040929     C*
015300040929     C                   IF        %EOF(FNFVV09L) OR FVVNFV > D55NRA
015400040929     C                   LEAVE
015500040929     C                   END
015600040929     c*
0157000409292    C                   IF        d55nrd <> D55NRA  or
015800040805     C                             (d55nrd = D55NRA  AND (D55NOT = ' ' OR
015900040805     C                             D55NBN = 0))
016000110520     C                   Z-ADD     FVVNFV        kfindc
016100110520     C     kfidst        CHAIN     Fidst09l
0162001105203    C                   IF        %FOUND(Fidst09L)
016300110520     C                   MOVEL     f_DSTNOT      D55NOT
016400040929E3   C                   END
016500041013     C                   Z-ADD     FVVNFV        qdtNFV
016600041013     C                   Z-ADD     FVVdfv        qdtddc
016700041013     C     kqdt          CHAIN     Fiqdt01L
0168000502073    C                   IF        %FOUND(Fiqdt01L) and qdtatb = ' '
016900041013     C                   Z-ADD     QDTNBNAF      D55NBN
017000050207     C                   MOVEL     QDTFNBAF      D55fba
017100041013E3   C                   END
017200040929E2   C                   END
017300050207     c* imposto decodifica in stampa del flag bancali affidati solo se = N
017400050207     c                   clear                   desban
017500050207     c                   if        d55fba = 'N'
017600050207     c                   eval      desban = 'NON AFFIDATI'
017700050207     c                   end
017800040929     C* Controllo se p.o. in gestione
017900941109     C     FVVFGS        LOOKUP    L6                                     40
018000040929     C                   IF        NOT *IN40
018100040929     C                   ITER
018200040929     C                   END
018300040929     C*
018400040930     C* AZZERO IL NUMERO DELLA PAGINA SOLO SE NON SONO STATO RICHIAMATO
018500040930     C* E DALLA QUADRATURA DISTINTA
018600040930     C                   IF        D55NWP = *BLANKS
018700040930     C                   Z-ADD     *ZEROS        D59PAG
018800040930     C                   END
018900911127     C                   Z-ADD     FVVNFV        NUMNFV            7 0
019000010215     C                   Z-ADD     FVVNFV        KNDIS             6 0
019100941109     C*  Controllo se foglio viaggio da considerare
019200040929     C     D55TIP        IFEQ      'T'
019300950102     C     D55TIP        OREQ      'C'
019400941109     C     FVVFCF        ANDNE     ' '
019500950102     C     D55TIP        OREQ      'A'
019600941109     C     FVVFCF        ANDEQ     ' '
019700040929     C                   ELSE
019800040929     C                   ITER
019900040929     C                   END
020000040929     C*
020100040929     C     FVVDFV        IFEQ      D55DAT
020200950102     C     NUMNFV        ANDLE     D55NRA
020300040929     C                   ELSE
020400040929     C                   ITER
020500040929     C                   END
020600040929     C* Controllo se esiste sul file FNARB
020700960702     C                   Z-ADD     NUMNFV        KNDC
020800960702     C                   Z-ADD     FVVDFV        KDDC
020900010604     C                   CLEAR                   FLGARB
021000030723     c* prima di iniziare il lopp su fnarb70l azzero arbnss per poi
021100030723     c* rinumerare in modo d'avere la stampa ordinata per spedizione
021200030723     c* visto che nella vista logica c'è davanti NSS
021300030723     c                   exsr      srazznss
021400010604      *--
021500040929     C     KARB          CHAIN     FNARB70L                           46
021600040929     C     *IN46         IFEQ      '0'
021700010604      * Flag per ok ARB
021800010604     C                   MOVEL     '1'           FLGARB            1
021900941109     C* Decodifico il padroncino
022000941109     C                   MOVEL     *BLANKS       APDRSC
022100040929     C     kapd          CHAIN     fiapd01L
022200040929     C                   MOVEL     APDRSC        STDPDR
022300040929     C                   MOVEL     APDPDR        STCPDR
022400040930     C                   MOVEL     APDRSC        prtdsf
022500050422     C                   MOVEL     APDRSC        prtdsf2
022600040930     C                   MOVEL     APDPDR        prtpdr
022700941109     C* Giro la data della distinta
022800941109     C                   CLEAR                   WLBDA8
022900941109     C                   Z-ADD     ARBDDC        G02INV
023000941109     C                   MOVEL     '3'           G02ERR
023100941109     C                   CALL      'XSRDA8'
023200941109     C                   PARM                    WLBDA8
023300040929     C                   Z-ADD     G02DAT        STCDDC
023400040929     C                   Z-ADD     NUMNFV        STCNDC
023500040930     C                   Z-ADD     NUMNFV        prtndt
023600110209     C                   move      d55bcd        prtndt2
023700110209     C                   z-add     numnfv        prtndt3
023800110209     C*                  eval      PRTNDT2=d55bcd
023900941228     C*
024000040929     C                   MOVEL     CMAT          TPPRE
024100941228     C     FVVFPP        IFEQ      'P'
024200040929     C                   MOVEL     CPOM          TPPRE
024300941228     C                   END
024400950126     C* Azzero Totali
024500950126     C                   EXSR      AZZTOT
024600080723     c* calcolo il totale ritiri perchè appare anche nei totali consegne
024700080723     C                   Z-ADD     0             OTPTRI            8 0          NUMERAT. RIGHE
024800080723     C     KFORM         SETLL     FNORM06L
024900080723     C                   DO        *HIVAL
025000080723     C     KFORM         READE     FNORM06L                               33
025100080723     C     *IN33         IFEQ      *ON
025200080723     C                   LEAVE
025300080723     C                   ENDIF
025400080723     c                   add       1             otptri
025500080723     c                   enddo
025600040929     C* Stampa distinta
025700010215     C     D55STP        IFNE      'O'
025800950126     C                   EXSR      STAMPA
025900010215     C                   ENDIF
026000040929E2   C                   END
026100040929      *--SE SCELTA LA STAMPA ANCHE DEGLI ORM O SOLO DEGLI ORM
0262000409292    C     D55STP        IFNE      'C'
026300010216      * Se non passa per fnarb devo decodifiare il padroncino e girare la data distinta
0264000409293    C     FLGARB        IFEQ      *BLANKS
026500010604      * Decodifico il padroncino
026600110520     C                   Z-ADD     FVVNFV        kfindc
026700110520     C     kfidst        CHAIN     Fidst09l
0268001105204    C                   IF        %FOUND(Fidst09l)
026900010216     C                   MOVEL     *BLANKS       APDRSC
027000040929     C     kapdd         CHAIN     fiapd01L
027100040929     C                   MOVEL     APDRSC        STDPDR
027200040929     C                   MOVEL     APDPDR        STCPDR
027300050110     C                   MOVEL     APDRSC        PRTDSF
027400050422     C                   MOVEL     APDRSC        PRTDSF2
027500050110     C                   MOVEL     APDPDR        PRTPDR
027600010216     C* Giro la data della distinta
027700010216     C                   CLEAR                   WLBDA8
027800010306     C                   Z-ADD     FVVDFV        G02INV
027900010216     C                   MOVEL     '3'           G02ERR
028000010216     C                   CALL      'XSRDA8'
028100010216     C                   PARM                    WLBDA8
028200040929     C                   Z-ADD     G02DAT        STCDDC
028300040929     C                   Z-ADD     NUMNFV        STCNDC
028400050110     C                   Z-ADD     NUMNFV        PRTNDT
028500110209     C*                  eval      PRTNDT2=d55bcd
028600110209     C                   move      d55bcd        prtndt2
028700040929E3   C                   ENDIF                                                  FLGARB
028800041013E4   C                   ENDIF                                                  *IN30
028900010215      * Stampa distinta ORM
029000040929     C                   EXSR      STPORM
029100040929E2   C                   ENDIF
029200041014     c* accendo indicatore per stampare la testata nel caso in cui ci siano
029300041014     c* altre distinte dopo
029400041014     c                   seton                                        80
029500941109     C*
029600040929     C                   ENDDO                                                  *IN45
029700911127     C*
029800950531     C                   SETON                                        RT
029900950126     C*-----------------------------------------------------------------
030000950126     C     AZZTOT        BEGSR
030100071022     C*                  Z-ADD     0             STPADR
030200071022     C                   Z-ADD     0             STPnocon
030300071022     C                   Z-ADD     0             STPsicon
030400950126     C                   Z-ADD     0             STPTSP
030500950126     C                   Z-ADD     0             STPTCO
030600061117     C                   Z-ADD     0             STPTCv
030700950126     C                   Z-ADD     0             STPTPK
030800130213     C                   Z-ADD     0             STPpkl
030900950126     C                   Z-ADD     0             STPTVL
031000950126     C                   Z-ADD     0             STPSTP
031100950126     C                   Z-ADD     0             STPTIC
031200990921     C                   Z-ADD     0             STPTID
031300990921     C                   Z-ADD     0             STPTEU
031400950126     C                   Z-ADD     0             STPTIV
031500010215      *--
031600010215     C                   Z-ADD     0             OTPTVL
031700010215      *--
031800950126     C*-----------------------------------------------------------------
031900950126     C                   ENDSR
032000950126     C*-----------------------------------------------------------------
032100911127     C     STAMPA        BEGSR
032200950602     C*
032300940323     C                   Z-ADD     0             XX
032400940323     C                   Z-ADD     0             STP
032500941109     C*
032600990920     C     *IN46         DOWEQ     '0'                                          -- 01 --
032700990927     C* Gesctisco azzeramento dati/importi
032800990927     C                   EXSR      AZZDAT
032900030723     C                   Z-ADD     WNSS          ARBNSS
033000030723     C                   EXCEPT    AGGNSS
033100990920     C     ARBFAN        IFEQ      'S'
033200040929     C                   MOVE      'A'           STDANO
033300990920     C                   END
033400040929      *TOTALIZZO NUMERO ADR
033500071022     C*    ARBTC1        IFEQ      'D'
033600071022     C*    ARBTC2        OREQ      'D'
033700071022     C*                  ADD       1             STPADR
033800071022     C*                  END
033900000516     C*
034000040929     C                   MOVEL     ARBRSD        STCDES
034100040929     C                   MOVEL     ARBRSM        STCMIT
034200081127     C                   EVAL      stclod = %TRIMR(arblod) + ' '
034300081127     C                             + arbprd
034400990920     C                   MOVEL     *BLANKS       STDTSP
034500990920     C     ARBTSP        IFEQ      'E'
034600091009     C     ARBTSP        oreq      'H'
034700040929     C                   MOVEL     ARBTSP        STDTSP
034800990920     C                   END
034900040929     C                   MOVE      ARBZNC        STCZNC
035000081117     C                   MOVE      ARBaas        STCaas
035100081117     C                   MOVE      ARBrmn        STCrmn
035200081117     C                   MOVE      ARBrma        STCrma
035300081117     C                   MOVE      ARBNRS        STCNRS
035400040929     C                   MOVE      ARBNRS        STCNRSC
035500040929     C                   MOVE      ARBFLS        STCFLS
035600040929     C                   MOVE      ARBNSP        STCNSP
035700040929     C                   MOVE      ARBLNP        STCLNP
035800040929     C                   MOVE      ARBNGD        STCNGD
035900041216     C*                  MOVE      ARBFIN        STCFIN
036000041216     C                   MOVE      ARBFIN        STDFIN
036100081117     c* ddt si solo se no parziale
036200081117     c                   if        arbdcp = 0
036300081117     C     KAR4          CHAIN     FiAR401L
036400081117     C                   IF        %found(fiar401l)
036500081117     C                   MOVEL     AR4NOT        DSBL4A
036600081117     c* stampati in arrivo
036700081117     c                   if        §b4abm='P' or §b4abm='Q' or
036800081117     c* stampati dal cliente o in partenza
036900081117     c                             §b4abm='C' or §b4abm='S'
037000081117     c* spedizioni mai messe in consegna ddt si
037100081117     c                   if        arbtmc = ' '
037200081117     c* stampo spedizione se 1° tentativo e solo per stampa anomalia
037300081117     C     d55tot        ifne      *blanks
037400081117     c* verifico se particolarità CD non andare in consegna senza ddt
037500081117     c                   if        arbgma <> ' '
037600081117     c                   exsr      srgma
037700081117     c                   end
037800081117     c                   seton                                        05
037900081117     C                   EXSR      STPDIS
038000081117     c                   endif
038100081117     c                   add       1             stpnocon
038200081117     c                   else
038300081117     c* spedizioni già messe in consegna ddt si
038400081117     c                   add       1             stpsicon
038500081117     c                   end
038600081117     C                   END
038700081117     c                   end
038800081117     C                   END
038900130213      *
039000990920     C* Controllo se bolla con mamma ....
039100990927     C                   EXSR      CTRMAM
039200990927     C* controllo il codice bolla
039300941110     C                   CLEAR                   DS3A
039400941110     C                   Z-ADD     1             X
039500941110     C     ARBCBO        LOOKUP    C3A(X)                                 32
039600941110     C     *IN32         IFEQ      '1'
039700950124     C                   MOVEL     D3A(X)        DS3A
039800941110     C                   END
039900941110     C*  Se non ho trovato contrassegno lo cerco su sped.
040000990921     C     AR9CAS        IFEQ      0
040100941110     C     §3AFCA        ANDNE     0
040200941110     C     ARBICC        IFEQ      'R'
040300941110     C     ARBICC        OREQ      *BLANKS
040400051115     C     KARB1         CHAIN     FiAR901L
040500051115     C                   IF        %found(fiar901l)
040600990921     C                   EXSR      IMPCAS
040700941110     C                   END
040800941110     C                   END
040900941110     C                   END
041000990927     C* Gestione dettaglio colli
041100990927     C                   EXSR      GESSGN
041200061120     c* verifico se merce di valore e sommo
041300061120     c                   eval      ar5trd = 'GEN'
041400061120     C     KARB5         CHAIN     FiAR501L
041500061120     C                   IF        %found(fiar501l)
041600061120     C                   eval      dar5gen = ar5uni
041700061120     c                   if        §AR5BVA = 'V'
041800061120     c                   add       stctco        stptcv
041900061120     C                   END
042000130320      * verifica se Packing list
042100130320     c                   if        §ar5als = 'S' or §ar5alx = 'S'
042200130320     c                   add       1             stppkl
042300130320     c                   endif
042400061120     C                   END
042500080723     c* calcola bancali a rendere e oltre 20 colli
042600080723     c                   exsr      srnban
042700070612     C* CALCOLO TOTALE PESO E VOLUME ANCORA DA CONSEGNARE con regole VDL
042800070612     c                   exsr      calpes
042900070612     c                   exsr      calvol
043000070612     C     wpes          DIV       ARBNCL        UNIKIL            8 3
043100990921     C     UNIKIL        MULT(H)   STCTCO        STCPKF
043200040929     C     UNIKIL        MULT(H)   STCTCO        STCPKFC
043300070612     C     wvol          DIV       ARBNCL        UNIVOL            9 5
043400990921     C     UNIVOL        MULT(H)   STCTCO        STCVLF
043500040929     C     UNIVOL        MULT(H)   STCTCO        STCVLFC
043600990927     C* Calcoli x stop
043700990921     C     ARBSTP        IFNE      *ZEROS
043800990921     C     ARBSTP        LOOKUP    STP                                    13
043900990921     C     *IN13         IFEQ      '0'
044000990921     C                   ADD       1             STPSTP
044100990921     C     XX            IFLT      999
044200990921     C                   ADD       1             XX                3 0
044300990921     C                   Z-ADD     ARBSTP        STP(XX)
044400990921     C                   END
044500990921     C                   END
044600990921     C                   END
044700990921     C     ARBSTP        IFEQ      *ZEROS
044800040929     C                   ADD       1             STPSTP
044900990921     C                   END
045000990921     C*
045100990921     C     ARBDCM        IFEQ      0
045200040929     C  N77              ADD       STCPKF        STPTPK
045300040929     C  N77              ADD       STCVLF        STPTVL
045400990921     C                   END
045500040929     C                   ADD       1             STPTSP
045600990921     C*
045700990921     C* EVIDENZIAZIONE PESO VOLUME
045800990921     C                   SETOFF                                       44
045900990921     C  N66              MOVEL     '3I'          TBLCOD
046000990921     C  N66              MOVEL     *BLANKS       TBLKEY
046100990921     C  N66              MOVEL     '1'           TBLKEY
046200990921     C  N66KTAB          CHAIN     TABEL                              11
046300990921     C  N11
046400990921     CANN66              MOVEL     TBLUNI        DS3I
046500990921     C  N66              SETON                                        66
046600990921     C  N77STCVLF        IFGT      §3IVOL
046700990921     C     ARBDCM        ANDEQ     0
046800990921     C     STCPKF        ORGT      §3IPKG
046900990921     C     ARBDCM        ANDEQ     0
047000990921     C                   SETON                                        44
047100990921     C                   END
047200990921     C*
047300990921     C* CALCOLO LE RIGHE CHE VENGONO USATE PER LA STAMPA
047400990921     C     ARBDCM        COMP      0                                  98
047500990921     C* Se ho valorizzato l'importo del C/Assegno effettuo stampa
047600990927     C     STCTPI        IFEQ      'C/A'
047700030625     C     d55tot        andeq     *blanks
047800040929     C                   SETON                                        81
047900040929     C                   EXSR      STPDIS
048000990921     C                   END
048100990921     C* Verifico se ci può essere un assegnato .....
048200911129     C                   MOVEL     §3ATB1        COM01             1
048300990921     C     COM01         IFEQ      'A'
048400990927     C     §3ATB2        ORNE      *BLANKS
048500990921     C* ... ancora da incassare
048600990921     C     ARBICA        IFEQ      ' '
048700990921     C     ARBICA        OREQ      'R'
048800990921     C                   EXSR      IMPASS
048900990921     C* ... stampo riga se no incassi
049000990921     C                   ELSE
049100990921     C     AR9CAS        IFEQ      0
049200030625     C     d55tot        andeq     *blanks
049300040929     C                   SETON                                        81
049400040929     C                   EXSR      STPDIS
049500990921     C                   END
049600990927     C                   END
049700990927     C                   ELSE
049800990927     C     AR9CAS        IFEQ      0
049900030625     C     d55tot        andeq     *blanks
050000040929     C                   SETON                                        81
050100040929     C                   EXSR      STPDIS
050200990921     C                   END
050300990921     C                   END
050400911209     C*
050500940517     C   95              DO
050600911127     C                   SETOFF                                       63
050700941110     C     KARB1         SETLL     FNART01L                               79
050800911127     C                   Z-ADD     1             S                 2 0
050900911127     C                   Z-ADD     *ZEROS        SCN
051000940517     C     *IN79         IFEQ      '1'
051100911127     C                   DO        *HIVAL                                       *B004
051200911127     C                   SETOFF                                       63
051300941110     C     KARB1         READE     FNART01L                               63
051400041013     C  N63S             DOWLe     13                                           *B005
051500930527     C     ARTDCM        IFEQ      *ZEROS
051600941110     C                   MOVE      ARTNSC        SCN(S)
051700911127     C                   ADD       1             S
051800041013     c                   if        s = 14
051900041013     c                   leave
052000041013     c                   end
052100930527     C                   END
052200911127     C                   SETOFF                                       63
052300941110     C     KARB1         READE     FNART01L                               63
052400911127     C  N63              END                                                    *E005
052500030625     c                   if        d55tot = *blanks
052600040929     C                   SETON                                        82
052700040929     C                   EXSR      STPDIS
052800030625     c                   end
052900911209     C*
053000911127     C                   Z-ADD     1             S
053100911127     C                   Z-ADD     *ZEROS        SCN
053200911127     C  N63              END                                                    *E004
053300940517     C                   ELSE                                                   *B003
053400940517     C     *IN96         IFEQ      '1'
053500941110     C     KLBL          SETLL     FNART01L
053600911127     C                   Z-ADD     1             S                 2 0
053700911127     C                   Z-ADD     *ZEROS        SCN
053800911127     C                   DO        *HIVAL                                       *B004
053900911127     C                   SETOFF                                       63
054000941110     C     KLBL          READE     FNART01L                               63
054100911127     C  N63S             DOWLE     13                                           *B005
054200930527     C     ARTDCM        IFEQ      *ZEROS
054300941110     C                   MOVE      ARTNSC        SCN(S)
054400911127     C                   ADD       1             S
054500041013     c                   if        s = 14
054600041013     c                   leave
054700041013     c                   end
054800930527     C                   END
054900911127     C                   SETOFF                                       63
055000941110     C     KLBL          READE     FNART01L                               63
055100911127     C  N63              END                                                    *E005
055200030625     c                   if        d55tot = *blanks
055300040929     C                   SETON                                        82
055400040929     C                   EXSR      STPDIS
055500030625     c                   end
055600911209     C*
055700911127     C                   Z-ADD     1             S
055800911127     C                   Z-ADD     *ZEROS        SCN
055900941110     C  N63              END
056000940517     C                   END                                                    *E003
056100940517     C                   END                                                    *E003
056200911127     C                   END                                                    *E002
056300960702     C                   Z-ADD     NUMNFV        KNDC
056400960702     C                   Z-ADD     FVVDFV        KDDC
056500020916     C     KARB          READE     FNARB70L                               46
056600911127     C  N50              END                                                    *DO *HIVAL
056700911127     C*
056800040930     C  N80LIN           IFGT      55
056900040929     C                   SETON                                        80
057000911209     C                   END
057100990927     C     STPTEU        COMP      0                                  94
057200030827     C     STPTID        COMP      0                                  92
057300040929     C                   SETON                                        83
057400040929     C                   EXSR      STPDIS
057500040929     C* SI/NO DISTINTA ANOMALIE
057600041013     C                   CLEAR                   DESANOM
057700040929     C     D55ANM        IFEQ      'S'
057800041013     C                   MOVEL(P)  STT(8)        DESANOM
057900040929     C                   END
058000950605     C     D55ANM        IFEQ      'N'
058100041013     C                   MOVE      STT(7)        DESANOM
058200040929     C                   END
058300040929     C                   SETON                                        84
058400040929     C                   EXSR      STPDIS
058500950911     C*
058600050518     C*    §7ASBR        IFNE      'N'
058700050518     C     *IN02         ifeq      '1'
058800050119     C     LIN           IFGT      51
058900040929     C                   SETON                                        80
059000950911     C                   END
059100040929     C                   SETON                                        85
059200040929     C                   EXSR      STPDIS
059300950925     C                   END
059400911127     C*
059500911127     C                   ENDSR
059600080723     C**********************************************************************
059700081114     C* aggancio tabella particolarità consegne ---------------------*
059800080723     C**********************************************************************
059900081114     C     srgma         BEGSR
060000081117     C                   MOVEL     '7R'          TBLCOD
060100081117     C                   MOVEL(p)  arbgma        TBLKEY
060200081114     c                   clear                   ds7r
060300081114     c     ktab          chain     tabel00f
060400081114     c                   if        %found(tabel00f)
060500081114     c                   movel     tbluni        ds7r
060600081117     c                   if        §7R9CD = 'N'
060700081117     c                   eval      stcdesp= 'Consegnare solo in presenza di DDT'
060800081114     c                   end
060900081117     c                   end
061000081114     C                   ENDSR
061100081114     C**********************************************************************
061200081114     C* calcolo il numero dei bancali   -----------------------------*
061300081114     C**********************************************************************
061400081114     C     srnban        BEGSR
061500080723      * Bancali
061600080723     c                   If        %Subst(Arbgva:1:1) = 'B'
061700080723     c                   Clear                   dAr5Ban
061800080723     C                   move      'BAN'         ar5trd
061900080723     c     karb5         Chain     Fiar501l
062000080723     c                   If        %Found(Fiar501l)
062100080723     c                   Movel     Ar5Uni        dAr5Ban
062200080723     c                   EndIf
062300080723     c                   add       §Ar5Nba       nban
062400080723     c                   add       §ar5Nb2       nban
062500080723     c                   EndIf
062600080723      * Bancali Bartolini
062700080723     c                   If        %Subst(Arbgva:1:1) = 'O'
062800080723     c                   Clear                   dAr5BNB
062900080723     C                   move      'BNB'         ar5trd
063000080723     c     karb5         Chain     Fiar501l
063100080723     c                   If        %Found(Fiar501l)
063200080723     c                   Movel     Ar5Uni        dAr5BNB
063300080723     c                   EndIf
063400080723     c                   add       §Ar5bNba      nban
063500080723     c                   add       §ar5bNb2      nban
063600080723     c                   EndIf
063700080723     c                   ENDSR
063800000606      *---------------------------------------------------------*
063900040929      *  compongo campone con segnacolli                        *
064000000606      *---------------------------------------------------------*
064100040929     C     sredit        BEGSR
064200040929     c* mi compongo un campone stcscn con i segnacolli editati Z e tra un
064300040929     c* segnacollo e l'altro ci aggiungo '/'
064400040930     c                   clear                   stcscn
064500040929     c                   do        13            nn                3 0
064600040929     c                   if        scn(nn) <> 0
064700040929     C                   EVAL      stcscn = %TRIMR(stcscn) +
064800040929     C                             %editc(scn(nn):'Z') + '/'
064900040929     c                   end
065000040929     c                   enddo
065100040929     C*
065200040929     C                   ENDSR
065300040929      *---------------------------------------------------------*
065400040929      *  CONTROLLO EVENTI PER ATTRIBUZIONE ANOMALIA             *
065500040929      *---------------------------------------------------------*
065600040929     C     EVENTI        BEGSR
065700000614     C                   SETON                                        68
065800000606     C     KARB1         SETGT     FNEVB05L
065900000606     C                   DO        *HIVAL
066000040929     C     KARB1         READPE    FNEVB05L
066100040929     c                   if        %eof(fnevb05l)
066200040929     C                   LEAVE
066300040929     c                   end
066400000606     C                   Z-ADD     1             X
066500000705     C     EVBCEV        LOOKUP    C2Z(X)                                 97
066600000705     C     *IN97         IFEQ      '1'
066700000606     C                   MOVEL     D2Z(X)        DS2Z
066800000606     C     §2ZCCA        IFNE      *BLANK
066900000606     C                   SETOFF                                           68
067000000606     C                   LEAVE
067100000606     C                   ELSE
067200000606     C                   ITER
067300000606     C                   END
067400000606     C                   END
067500000606     C                   ENDDO
067600000606     C                   ENDSR
067700990927      *--------------------------------------------------------------*
067800990927      * Azzeramento dati x impostazione stampa                       *
067900990927      *--------------------------------------------------------------*
068000990927     C     AZZDAT        BEGSR
068100990927     C*
068200990927     C                   SETOFF                                       9496
068300990927     C                   SETOFF                                       9392
068400990927     C                   MOVE      *BLANKS       COMFNS            1
068500990927     C                   MOVEL     *BLANKS       STCVAL
068600990927     C                   MOVEL     *BLANKS       STCTPI
068700990927     C                   MOVEL     'N'           WRTDET            1
068800990927     C                   Z-ADD     0             AR9CAS
068900990927     C                   Z-ADD     0             STCPKF
069000040929     C                   Z-ADD     0             STCPKFC
069100990927     C                   Z-ADD     0             STCVLF
069200040929     C                   Z-ADD     0             STCVLFC
069300990927     C                   Z-ADD     0             STCIMP
069400990927     C                   Z-ADD     0             STCIPD
069500040929     C                   Z-ADD     0             STCIPV
069600040929     C                   Z-ADD     0             STCIVL
069700030723     C                   ADD       1             WNSS              3 0
069800990927     C*
069900990927     C                   ENDSR
070000990927      *--------------------------------------------------------------*
070100990927      * CTRMAM: Controllo se bolla con mamma                         *
070200990927      *--------------------------------------------------------------*
070300990927     C     CTRMAM        BEGSR
070400990927     C*
070500031118     c                   clear                   arbccas
070600990927     C* Controllo se bolla con mamma ....
070700990927     C     KARB1         CHAIN     FNLBL01L                           54
070800990927     C     *IN54         IFEQ      '0'                                          _ 02 _
070900990927     C     LBLLAN        ANDEQ     LBLLAP
071000990927     C     KLBL          CHAIN     FNARB01L                           56
071100990927     C* ... no reso (Riconsegna)
071200990927     C     *IN56         IFEQ      '0'                                          _ 03 _
071300990927     C     ARBCCA        ANDNE     '2'                                          _ 03 _
071400990927     C     ARBCCA        ANDNE     '6'                                          _ 03 _
071500990927     C* Stampo sgn da mamma
071600990927     C                   SETON                                        96         C'E' LA MAMMA
071700990927     C                   MOVE      ARBFNS        COMFNS            1
071800040929     C                   Z-ADD     ARBNCD        STCNCD
071900040929     C                   Z-ADD     ARBNCA        STCNCA
072000990927     C* Controllo se c'è C/Assegno da incassare
072100990927     C     ARBICC        IFEQ      ' '                                          _ 04 _
072200990927     C     ARBICC        OREQ      'R'
072300990927     C* controllo il codice bolla per vedere se c'è il Contrassegno
072400990927     C                   Z-ADD     1             X
072500990927     C     ARBCBO        LOOKUP    C3A(X)                                 32
072600990927     C     *IN32         IFEQ      '1'                                          _ 05 _
072700990927     C                   MOVEL     D3A(X)        DS3A
072800990927     C     §3AFCA        IFNE      0                                            _ 06 _
072900051115     C     KLBL          CHAIN     FiAR901L                           32
073000990927     C     *IN32         IFEQ      '0'                                          _ 07 _
073100990927     C                   EXSR      IMPCAS
073200990927     C                   END                                                    _ 07 _
073300990927     C                   END                                                    _ 06 _
073400990927     C                   END                                                    _ 05 _
073500990927     C                   END                                                    _ 04 _
073600990927     C*
073700031118     C                   ELSE
073800031118     C  N56              EVAL      ARBCCAS=ARBCCA
073900990927     C                   END                                                    _ 03 _
074000990927     C* RIAGGANCIO LA FIGLIA
074100990927     C     KLBL1         CHAIN     FNARB01L                           50
074200031118     C* SE LA BOLLA MAMMA È UN RESO MEMORIZZO I SEGNACOLLI DELLA FIGLIA
074300031118     C                   IF        NOT *IN50 AND (ARBCCAS='2' OR
074400031118     C                             ARBCCAS = '6')
074500031118     C                   Z-ADD     ARBNCD        STCNCD
074600031118     C                   Z-ADD     ARBNCA        STCNCA
074700031118     C                   END
074800990927     C                   ELSE                                                   _ 02 _
074900990927     C                   Z-ADD     ARBNCD        STCNCD
075000990927     C                   Z-ADD     ARBNCA        STCNCA
075100990927     C                   END                                                    _ 02 _
075200990927     C*
075300990927     C                   ENDSR
075400990927      *--------------------------------------------------------------*
075500990927      * GESSGN: Gestione dettaglio segnacolli                        *
075600990927      *--------------------------------------------------------------*
075700990927     C     GESSGN        BEGSR
075800990927     C*
075900990927     C                   Z-ADD     0             STCTCO
076000990927     C                   SETOFF                                       77
076100990927     C     ARBDCM        IFEQ      0                                             -- 02 --
076200990927     C     KARB1         SETLL     FNART01L                               79
076300990927     C   79              SETOFF                                       78
076400990927     C  N79              SETON                                        7778
076500990927     C     *IN78         IFEQ      '0'                                           -- 03 --
076600990927     C     *IN78         DOWEQ     '0'                                           -- 04 --
076700990927     C     KARB1         READE     FNART000                               78
076800990927     C*
076900990927     C  N78ARTDCM        IFEQ      0                                             -- 05 --
077000040929     C                   ADD       1             STPTCO
077100040929     C                   ADD       1             STCTCO
077200990927     C                   ELSE
077300990927     C* C'E' SEGNAC.CONSEGNATO =PARZIALE - LO TRATTO COME NON SEQ.
077400990927     C                   SETON                                        94
077500990927     C                   END                                                     -- 05 --
077600990927     C*
077700990927     C                   END                                                     -- 04 --
077800990927     C*
077900990927     C                   ELSE                                                    -- 03 --
078000990927     C*
078100990927     C     *IN54         IFEQ      '0'                                           -- 04 --
078200990927     C     KLBL          SETLL     FNART01L                               79
078300990927     C   79              SETOFF                                       7778
078400990927     C  N79              SETON                                        7778
078500990927     C     *IN78         DOWEQ     '0'                                           -- 05 --
078600990927     C     KLBL          READE     FNART000                               78
078700990927     C*
078800990927     C  N78ARTDCM        IFEQ      0                                             -- 06 --
078900040929     C                   ADD       1             STPTCO
079000040929     C                   ADD       1             STCTCO
079100990927     C                   ELSE
079200990927     C* C'E' SEGNAC.CONSEGNATO =PARZIALE - LO TRATTO COME NON SEQ.
079300990927     C                   SETON                                        94
079400990927     C                   END                                                     -- 06 --
079500990927     C*
079600990927     C                   END                                                     -- 05 --
079700990927     C                   END                                                     -- 04 --
079800990927     C                   END                                                     -- 03 --
079900990927     C                   END                                                     -- 02 --
080000990927     C*
080100990927     C** NUMERO COLLI = 1 X X-CUMULATIVO
080200990927     C  N54
080300990927     CANN56              MOVE      COMFNS        ARBFNS
080400990927     C   94              MOVEL     'S'           ARBFNS                         CONS.PARZIALE
080500990927     C                   SETOFF                                       95
080600990927     C     ARBFNS        IFEQ      'S'                                          *B003
080700990927     C     ARBDCM        ANDEQ     0
080800990927     C                   SETON                                        95
080900990927     C                   END                                                    *E003
081000990927     C*
081100990927     C                   ENDSR
081200990921      *--------------------------------------------------------------*
081300990921      * IMPCAS: Imposto dati importo C/Ass.                          *
081400990921      *--------------------------------------------------------------*
081500990921     C     IMPCAS        BEGSR
081600990921     C*
081700990921     C* Se c'è C/Assegno imposto tipo incasso con C/A
081800990921     C                   MOVEL     'C/A'         STCTPI
081900990921     C* ... verifico se è in moneta Corrente
082000011009     C     AR9VCA        IFEQ      §GEDCR                                       -- 01 --
082100990924     C     DECIM         IFEQ      'S'
082200990927     C                   SETON                                        92
082300040929     C                   ADD       AR9CAS        STPTID
082400040929     C                   Z-ADD     AR9CAS        STCIPD
082500990921     C                   ELSE
082600990927     C                   SETOFF                                       92
082700040929     C                   ADD       AR9CAS        STPTIC
082800040929     C                   Z-ADD     AR9CAS        STCIMP
082900990927     C                   END
083000990921     C* Se importo in moneta corrente converto in altra moneta
083100990921     C* valida se c'è
083200990921     C     DIVEUR        IFNE      *BLANKS
083300990921     C                   CLEAR                   YEURDS
083400990921     C                   MOVEL     AR9VCA        YECDVI
083500990921     C                   Z-ADD     AR9CAS        YECIMI
083600990927     C                   MOVEL     DIVEUR        YECDVO
083700990921     C                   MOVEL     'H'           YECTAR
083800990921     C                   CALL      'YEURCO'
083900990921     C                   PARM                    YEURDS
084000040929     C                   ADD       YECIMO        STPTEU
084100040929     C                   MOVEL     DIVEUR        STCVAL
084200990927     C* Imposto controvalore
084300990927     C     DECEUR        IFEQ      'S'
084400990927     C                   SETOFF                                       93
084500990927     C                   Z-ADD     YECIMO        STCIPV
084600990927     C                   ELSE
084700990927     C                   SETON                                        93
084800990927     C                   Z-ADD     YECIMO        STCIVL
084900990921     C                   END
085000990927     C                   END
085100990921     C* Se l'importo non è in moneta corrente eseguo conversione
085200990921     C                   ELSE
085300990921     C* Solo se la divisa è non è della moneta corrente ma è relativa
085400990921     C* all'altra divisa valida per l'incasso converto l'importo nella
085500990921     C* divisa corrente e lo espongo in stampa
085600990921     C     DIVEUR        IFEQ      AR9VCA
085700990921     C                   CLEAR                   YEURDS
085800040929     C                   MOVEL     AR9VCA        STCVAL
085900990921     C                   MOVEL     AR9VCA        YECDVI
086000990921     C                   Z-ADD     AR9CAS        YECIMI
086100011009     C                   MOVEL     §GEDCR        YECDVO
086200990921     C                   MOVEL     'H'           YECTAR
086300990921     C                   CALL      'YEURCO'
086400990921     C                   PARM                    YEURDS
086500040929     C                   ADD       AR9CAS        STPTEU
086600990927     C     DECEUR        IFEQ      'S'
086700990927     C                   SETOFF                                       93
086800990927     C                   Z-ADD     AR9CAS        STCIPV
086900990927     C                   ELSE
087000990927     C                   SETON                                        93
087100990927     C                   Z-ADD     AR9CAS        STCIVL
087200990927     C                   END
087300990927     C     DECIM         IFEQ      'S'
087400990927     C                   SETON                                        92
087500040929     C                   ADD       YECIMO        STPTID
087600030827     C                   Z-ADD     YECIMO        STCIPD
087700990927     C                   ELSE
087800990927     C                   SETOFF                                       92
087900040929     C                   ADD       YECIMO        STPTIC
088000990927     C                   Z-ADD     YECIMO        STCIMP
088100990927     C                   END
088200990921     C* ... altrimenti azzero importi in divisa corrente
088300990921     C                   ELSE
088400990927     C                   SETOFF                                       93
088500040929     C                   MOVEL     AR9VCA        STCVAL
088600990927     C                   ADD       AR9CAS        STCIPV
088700040929     C                   ADD       AR9CAS        STPTIV
088800990921     C                   END
088900990921     C                   END
089000990921     C*
089100990921     C                   ENDSR
089200990921      *--------------------------------------------------------------*
089300990921      * IMPASS: Gestisce eventuale stampa degli assegnati            *
089400990921      *--------------------------------------------------------------*
089500990921     C     IMPASS        BEGSR
089600990921     C*
089700990921     C* Aggancio file tassazione con flag impostato x seconda bolla
089800990921     C     §3ATB2        IFNE      *BLANKS
089900990921     C                   MOVEL     '2'           KTRC
090000990921     C                   ELSE
090100990921     C                   MOVEL     '1'           KTRC
090200990921     C                   END
090300990921     C                   Z-ADD     ARBAAS        KAAS
090400990921     C                   Z-ADD     ARBLNP        KLNP
090500990921     C                   Z-ADD     ARBNRS        KNRS
090600990921     C                   Z-ADD     ARBNSP        KNSP
090700990921     C     KAR6          CHAIN     FIAR601L                           32
090800990921     C* Devo incassare assegnato
090900990927     C     *IN32         IFEQ      '0'                                          -- 01 --
091000990921     C     AR6DFT        ANDGT     *ZEROS
091100990921     C                   Z-ADD     AR6IFT        IMPRES
091200990921     C* Se è stato effettuato precedentemente un incasso parziale
091300990921     C* aggancio file degli incassi x eseguire calcolo totale da
091400990921     C* incassare
091500990921     C     ARBFIP        IFEQ      'I'                                          -- 02 --
091600990921     C                   MOVEL     'A'           KTIP
091700990921     C                   Z-ADD     AR6IFT        IMPRES
091800990921     C     KARI          CHAIN     FIARI01L                           32
091900990921     C* Se esiste record determino importo residuo da incassare:
092000991011     C     *IN32         DOWEQ     '0'                                          -- 03 --
092100990921     C* Se importo incass. e da incassare hanno la stessa divisa
092200990921     C* eseguo la differenza
092300990921     C     AR6DIV        IFEQ      ARIDIV                                       -- 04 --
092400990921     C                   SUB       ARIIPP        IMPRES           15 3
092500990921     C* Se la divisa di incasso è diversa converto importo già
092600990921     C* incassato nella divisa della bolla
092700990921     C                   ELSE
092800990921     C                   CLEAR                   YEURDS
092900990921     C                   MOVEL     ARIDIV        YECDVI
093000990921     C                   Z-ADD     ARIIPP        YECIMI
093100990927     C                   MOVEL     AR6DIV        YECDVO
093200990921     C                   MOVEL     'H'           YECTAR
093300990921     C                   CALL      'YEURCO'
093400990921     C                   PARM                    YEURDS
093500990921     C                   SUB       YECIMO        IMPRES
093600990921     C                   END                                                    -- 04 --
093700990921     C     KARI          READE     FIARI01L                               32
093800990921     C                   END                                                    -- 03 --
093900990921     C*
094000990921     C                   END                                                    -- 02 --
094100990921     C*
094200990921     C* Calcolo importi assegnato x stp
094300040929     C                   MOVEL     'Ass'         STCTPI
094400990921     C* ... verifico se è in moneta Corrente
094500011009     C     AR6DIV        IFEQ      §GEDCR                                       -- 02 --
094600990921     C     DECIM         IFEQ      'S'
094700990921     C                   SETON                                        92
094800040929     C                   Z-ADD     IMPRES        STCIPD
094900040929     C                   ADD       IMPRES        STPTID
095000990921     C                   ELSE
095100990921     C                   SETOFF                                       92
095200990927     C                   Z-ADD     IMPRES        STCIMP
095300040929     C                   ADD       IMPRES        STPTIC
095400990921     C                   END
095500990921     C* Se importo in moneta corrente converto in altra moneta
095600990921     C* valida se c'è
095700990921     C     DIVEUR        IFNE      *BLANKS                                      -- 03 --
095800990921     C                   CLEAR                   YEURDS
095900990921     C                   MOVEL     AR6DIV        YECDVI
096000990921     C                   Z-ADD     IMPRES        YECIMI
096100990927     C                   MOVEL     DIVEUR        YECDVO
096200990921     C                   MOVEL     'H'           YECTAR
096300990921     C                   CALL      'YEURCO'
096400990921     C                   PARM                    YEURDS
096500040929     C                   ADD       YECIMO        STPTEU
096600040929     C                   MOVEL     DIVEUR        STCVAL
096700990927     C* Imposto controvalore
096800990927     C     DECEUR        IFEQ      'S'
096900990927     C                   SETOFF                                       93
097000990927     C                   Z-ADD     YECIMO        STCIPV
097100990927     C                   ELSE
097200990927     C                   SETON                                        93
097300990927     C                   Z-ADD     YECIMO        STCIVL
097400990927     C                   END
097500990927     C                   END                                                    -- 03 --
097600990921     C* Se l'importo non è in moneta corrente eseguo conversione
097700990921     C                   ELSE                                                   -- 02 --
097800990921     C* Solo se la divisa è non è della moneta corrente ma è relativa
097900990921     C* all'altra divisa valida per l'incasso converto l'importo nella
098000990921     C* divisa corrente e lo espongo in stampa
098100990921     C     DIVEUR        IFEQ      AR6DIV                                       -- 03 --
098200990921     C                   CLEAR                   YEURDS
098300040929     C                   MOVEL     AR6DIV        STCVAL
098400990921     C                   MOVEL     AR6DIV        YECDVI
098500990921     C                   Z-ADD     IMPRES        YECIMI
098600011009     C                   MOVEL     §GEDCR        YECDVO
098700990921     C                   MOVEL     'H'           YECTAR
098800990921     C                   CALL      'YEURCO'
098900990921     C                   PARM                    YEURDS
099000990927     C     DECEUR        IFEQ      'S'
099100990927     C                   SETOFF                                       93
099200990927     C                   Z-ADD     IMPRES        STCIPV
099300990927     C                   ELSE
099400990927     C                   SETON                                        93
099500990927     C                   Z-ADD     IMPRES        STCIVL
099600990927     C                   END
099700990927     C     DECIM         IFEQ      'S'
099800990921     C                   SETON                                        92
099900040929     C                   Z-ADD     YECIMO        STCIPD
100000040929     C                   ADD       YECIMO        STPTID
100100990927     C                   ELSE
100200990927     C                   SETOFF                                       92
100300990927     C                   Z-ADD     YECIMO        STCIMP
100400990927     C                   ADD       YECIMO        STPTIC
100500990927     C                   END
100600040929     C                   ADD       IMPRES        STPTEU
100700990921     C* ... altrimenti azzero importi in divisa corrente
100800990921     C                   ELSE
100900990921     C                   SETOFF                                       92
101000990927     C                   Z-ADD     0             STCIMP
101100040929     C                   ADD       IMPRES        STPTIV
101200040929     C                   Z-ADD     YECIMO        STCIPD
101300990921     C                   END                                                    -- 03 --
101400990921     C* Imposto dati divisa di incasso
101500990921     C                   MOVEL     AR6DIV        STCVAL
101600990921     C                   END                                                    -- 02 --
101700990921     C*
101800990921     C* Verifico se devo stampare seconda riga
101900030625     c                   if        d55tot = *blanks
102000990921     C     AR9CAS        IFNE      0
102100040929     C                   SETON                                        86
102200990921     C                   ELSE
102300040929     C                   SETON                                        81
102400990921     C                   END
102500040929     C                   EXSR      STPDIS
102600030625     C                   END
102700990921     C* ... stampo riga se no incassi
102800990921     C                   ELSE
102900990921     C     AR9CAS        IFEQ      0
103000030625     C     d55tot        andeq     *blanks
103100040929     C                   SETON                                        81
103200040929     C                   EXSR      STPDIS
103300990921     C                   END
103400990921     C                   END
103500990921     C*
103600990921     C                   ENDSR
103700010215      *--
103800010215      *--------------------------------------------------------------*
103900040929      * STPdis: Stampa distinta                                      *
104000010215      *--------------------------------------------------------------*
104100040929     C     STPdis        BEGSR
104200040930     C                   IF        *IN80
104300040930     C                   ADD       1             D59PAG
104400040930     C                   write     TESTa
104500050502     c                   if        d55lsr = 'S' and d59pag = 1
104600050422     C                   write     testal
104700050426     c                   end
104800050119     C  N85              write     TESTS3
104900040930     C  N04              write     TESTS4
105000081117     c* spedizioni ddt si
105100081117     C   04              write     TESTS5
105200040929     C                   SETOFF                                       80
105300040930     C                   END
105400040929     C   81              write     DETTS
105500040929     c   82              exsr      sredit
105600040929     C   82              write     DETSCN
105700081117     C   05              write     DETTS5
105800040929     C   83              write     TOTALE
105900040929     C   84              write     SINOANOM
106000050119     C* STAMPA SOLO LA PARTE DEL RIENTRO SE RICHIESTA LA FIRMA
106100050119     C                   IF        *IN85 AND *IN02
106200050119     C                   write     RIENTR
106300050119     C                   END
106400040929     C   86              write     IMPS
106500040929     C                   EXSR      CLRFLD
106600040929     C                   SETOFF                                       818283
106700040929     C                   SETOFF                                       848586
106800081117     c                   setoff                                       05
106900040929     C                   ENDSR
107000040929      *--
107100040929      *--------------------------------------------------------------*
107200040929      * CLRFLD  PULISCE I CAMPI IN STAMPA                            *
107300040929      *--------------------------------------------------------------*
107400040929     C     CLRFLD        BEGSR
107500040929     C* DOPO AVER STAMPATO LA RIGA DI DETTAGLIO PULISCO ALCUNI CAMPI
107600081117     C                   IF        *IN81 or *in05
107700040929     C                   CLEAR                   STDANO
107800040929     C                   CLEAR                   STDTSP
107900040929     C                   CLEAR                   STCZNC
108000040929     C                   CLEAR                   STCFLS
108100040929     C                   CLEAR                   STCDES
108200081127     C                   CLEAR                   STClod
108300040929     C                   CLEAR                   STCMIT
108400040929     C                   CLEAR                   STCLNP
108500040929     C                   CLEAR                   STCNRS
108600040929     C                   CLEAR                   STCNSP
108700081117     C                   CLEAR                   STCaas
108800081117     C                   CLEAR                   STCdesp
108900081117     C                   CLEAR                   STCrmn
109000081117     C                   CLEAR                   STCrma
109100040929     C                   CLEAR                   STCNGD
109200041216     C*                  CLEAR                   STCFIN
109300041216     C                   CLEAR                   STDFIN
109400040929     C                   IF        NOT *IN95 AND NOT *IN98
109500040929     C                   CLEAR                   STCNCD
109600040929     C                   END
109700040929     C                   IF        NOT *IN77 AND NOT *IN98
109800040929     C                   CLEAR                   STCTCO
109900040929     C                   END
110000040929     C                   IF        NOT *IN92
110100040929     C                   CLEAR                   STCIMP
110200040929     C                   END
110300040929     C                   CLEAR                   STCVAL
110400040929     C                   IF        NOT *IN93
110500040929     C                   CLEAR                   STCIPV
110600040929     C                   ELSE
110700040929     C                   CLEAR                   STCIVL
110800040929     C                   END
110900040929     C                   END
111000040929     C* DOPO AVER STAMPATO LA RIGA DEGLI IMPORTI PULISCO ALCUNI CAMPI
111100040929     C                   IF        *IN86
111200040929     C                   CLEAR                   STCVAL
111300040929     C                   IF        NOT *IN92
111400040929     C                   CLEAR                   STCIMP
111500040929     C                   ELSE
111600040929     C                   CLEAR                   STCIPD
111700040929     C                   END
111800040929     C                   IF        NOT *IN93
111900040929     C                   CLEAR                   STCIPV
112000040929     C                   ELSE
112100040929     C                   CLEAR                   STCIVL
112200040929     C                   END
112300040929     C                   END
112400040929     C                   ENDSR
112500040929      *--
112600040929      *--------------------------------------------------------------*
112700040929      * STPORM: Stampa distinta ORM                                  *
112800040929      *--------------------------------------------------------------*
112900040929     C     STPORM        BEGSR
113000010215      *
113100080401      * se richiesta stampa dei solo ORM verifico se valorizzato il flag
113200080401      * della stampa distinte su pda oppure no e solo se la distinta
113300080401      * è su pda
113400080401     c                   if        d55stp = 'O' and d55pda <> 'S'
113500080401      * verifico se in distinta il flag "Dati per PDA" = "O" od "E" (DSTPDA del file FIDST00F) e
113600080401      *"test per PDA" <> "O" ed "E" (§DSTTSTPDA ds DDSTFLR relativa al campo DSTFLR stesso file)
113700110520     c                   z-add     fvvnfv        kfindc
113800080401     c     kfidst        chain     fidst09l
113900080401     c                   if        %found(fidst09l)
114000080401     c                   movel     f_dstflr      ddstflr
114100080401      * distinta su PDA
114200080401     c                   If        (f_dstpda = 'O' or f_dstpda = 'E') and
114300080401      * verifico se è in test
114400080401     c                             (§dsttstpda <> 'O' and §dsttstpda <> 'E')
114500080401      * escluso la distinta
114600080401     c                   leavesr
114700080401
114800080401     c                   endif
114900080401     c                   endif
115000080401
115100080401     c                   endif
115200080401
115300080401     C                   SETOFF                                       8880
115400080401     C                   Z-ADD     0             OTPTRI            8 0          NUMERAT. RIGHE
115500010215      *
115600010215     C     KFORM         SETLL     FNORM06L
115700010215     C                   DO        *HIVAL
115800010215     C     KFORM         READE     FNORM06L                               33
115900010215     C     *IN33         IFEQ      *ON
116000010215     C                   LEAVE
116100010215     C                   ENDIF
116200010215      *
116300010215     C     *IN88         IFEQ      *OFF
116400041014     C                   ADD       1             D59PAG
116500041013     C                   write     TESTOR
116600010215     C                   SETON                                        88
116700010215     C                   ENDIF
116800010215      *
116900011107     C                   CLEAR                   OTPLOR
117000111013     C                   CLEAR                   OTPLOC
117100010215     C                   CLEAR                   OTPQT1
117200010215     C                   CLEAR                   OTPQT2
117300010215     C                   CLEAR                   OTPQT3
117400010215     C                   CLEAR                   OTPUM1
117500010215     C                   CLEAR                   OTPUM2
117600010215     C                   CLEAR                   OTPUM3
117700010215     C                   CLEAR                   TPTVL
117800011107     C                   CLEAR                   OTPPRE
117900011107      * Località + provincia
118000011107     C     ORMLOR        CAT       ORMPRR:1      OTPLOR           38
118100111013      * Località + provincia destinatario  o nazione se presente
118200111013     c                   IF        ORMNAC <> *blanks
118300111013     C     ORMLOC        CAT       ORMNAC:1      OTPLOC           39
118400111013     c                   ELSE
118500111013     C     ORMLOC        CAT       ORMPRC:1      OTPLOC           39
118600111013     c                   ENDIF
118700111013     C                   CLEAR                   OTPPRE
118800010215      * Prepagato
118900010215     C     ORMTOR        IFEQ      'P'
119000010215     C                   MOVEL     'PREP.'       OTPPRE            5
119100010215     C                   ENDIF
119200010215      * Quantità e Unità di Misura
119300010215     C     ORMBLC        ADD       ORMMTC        TOTQTA            5 0
119400010215     C                   ADD       ORMATT        TOTQTA
119500010215      * Colli
119600010215     C     ORMNCL        IFGT      0
119700010215     C                   Z-ADD     ORMNCL        OTPQT1            6 3
119800010215     C                   MOVE      'cl'          OTPUM1            2
119900010215     C                   ENDIF
120000010215      * Peso
120100010215     C     ORMPKG        IFGT      0
120200010215     C     OTPQT1        ANDEQ     0
120300010215     C                   Z-ADD     ORMPKG        OTPQT1
120400010215     C                   MOVE      'kg'          OTPUM1
120500010215     C                   GOTO      VLM
120600010215     C                   ENDIF
120700010215     C     ORMPKG        IFGT      0
120800010215     C     OTPQT2        ANDEQ     0
120900010215     C                   Z-ADD     ORMPKG        OTPQT2            6 3
121000010215     C                   MOVE      'kg'          OTPUM2            2
121100010215     C                   ENDIF
121200010215      * Volume
121300010215     C     VLM           TAG
121400010215     C     ORMVLM        IFGT      0
121500010215     C     OTPQT1        ANDEQ     0
121600010215     C                   Z-ADD     ORMVLM        OTPQT1
121700010215     C                   MOVE      'vl'          OTPUM1
121800010215     C                   GOTO      BNC
121900010215     C                   ENDIF
122000010215     C     ORMVLM        IFGT      0
122100010215     C     OTPQT2        ANDEQ     0
122200010215     C                   Z-ADD     ORMVLM        OTPQT2
122300010215     C                   MOVE      'vl'          OTPUM2
122400010215     C                   GOTO      BNC
122500010215     C                   ENDIF
122600010215     C     ORMVLM        IFGT      0
122700010215     C     TOTQTA        ANDEQ     0
122800010215     C                   Z-ADD     ORMVLM        OTPQT3            6 3
122900010215     C                   MOVE      'vl'          OTPUM3            2
123000010215     C                   ENDIF
123100010215      * Bancali
123200010215     C     BNC           TAG
123300010215     C     ORMBNC        IFGT      0
123400010215     C     OTPQT1        ANDEQ     0
123500010215     C                   Z-ADD     ORMBNC        OTPQT1
123600010215     C                   MOVE      'bn'          OTPUM1
123700010215     C                   GOTO      BLC
123800010215     C                   ENDIF
123900010215     C     ORMBNC        IFGT      0
124000010215     C     OTPQT2        ANDEQ     0
124100010215     C                   Z-ADD     ORMBNC        OTPQT2
124200010215     C                   MOVE      'bn'          OTPUM2
124300010215     C                   GOTO      BLC
124400010215     C                   ENDIF
124500010215     C     ORMBNC        IFGT      0
124600010215     C     TOTQTA        ANDEQ     0
124700010215     C                   Z-ADD     ORMBNC        OTPQT3
124800010215     C                   MOVE      'bn'          OTPUM3
124900010215     C                   ENDIF
125000010215      * Bilici
125100010215     C     BLC           TAG
125200010215     C     ORMBLC        IFGT      0
125300010215     C     OTPQT1        ANDEQ     0
125400010215     C                   Z-ADD     ORMBLC        OTPQT1
125500010215     C                   MOVE      'bl'          OTPUM1
125600010215     C                   GOTO      MTC
125700010215     C                   ENDIF
125800010215     C     ORMBLC        IFGT      0
125900010215     C     OTPQT2        ANDEQ     0
126000010215     C                   Z-ADD     ORMBLC        OTPQT2
126100010215     C                   MOVE      'bl'          OTPUM2
126200010215     C                   GOTO      MTC
126300010215     C                   ENDIF
126400010215     C     ORMBLC        IFGT      0
126500010215     C     OTPQT3        ANDEQ     0
126600010215     C                   Z-ADD     ORMBLC        OTPQT3
126700010215     C                   MOVE      'bl'          OTPUM3
126800010215     C                   ENDIF
126900010215      * Motrici
127000010215     C     MTC           TAG
127100010215     C     ORMMTC        IFGT      0
127200010215     C     OTPQT1        ANDEQ     0
127300010215     C                   Z-ADD     ORMMTC        OTPQT1
127400010215     C                   MOVE      'mt'          OTPUM1
127500010215     C                   GOTO      ATT
127600010215     C                   ENDIF
127700010215     C     ORMMTC        IFGT      0
127800010215     C     OTPQT2        ANDEQ     0
127900010215     C                   Z-ADD     ORMMTC        OTPQT2
128000010215     C                   MOVE      'mt'          OTPUM2
128100010215     C                   GOTO      ATT
128200010215     C                   ENDIF
128300010215     C     ORMMTC        IFGT      0
128400010215     C     OTPQT3        ANDEQ     0
128500010215     C                   Z-ADD     ORMMTC        OTPQT3
128600010215     C                   MOVE      'mt'          OTPUM3
128700010215     C                   ENDIF
128800010215      * Autotreni
128900010215     C     ATT           TAG
129000010215     C     ORMATT        IFGT      0
129100010215     C     OTPQT1        ANDEQ     0
129200010215     C                   Z-ADD     ORMATT        OTPQT1
129300010215     C                   MOVE      'au'          OTPUM1
129400010215     C                   GOTO      NOQTA
129500010215     C                   ENDIF
129600010215     C     ORMATT        IFGT      0
129700010215     C     OTPQT2        ANDEQ     0
129800010215     C                   Z-ADD     ORMATT        OTPQT2
129900010215     C                   MOVE      'au'          OTPUM2
130000010215     C                   GOTO      NOQTA
130100010215     C                   ENDIF
130200010215     C     ORMATT        IFGT      0
130300010215     C     OTPQT3        ANDEQ     0
130400010215     C                   Z-ADD     ORMATT        OTPQT3
130500010215     C                   MOVE      'au'          OTPUM3
130600010215     C                   ENDIF
130700010215     C     NOQTA         TAG
130800010215      * Calcolo tot.volume
130900010215     C     ORMVLM        IFNE      0
131000010215     C                   ADD       ORMVLM        OTPTVL            6 3
131100010215     C                   ELSE
131200010215     C     ORMPKG        IFNE      0
131300010215     C     ORMPKG        DIV(H)    200           TPTVL             6 3
131400010215     C                   ADD       TPTVL         OTPTVL
131500010215     C                   ELSE
131600010215     C     ORMBNC        IFNE      0
131700010215     C                   ADD       ORMBNC        OTPTVL
131800010215     C                   ENDIF
131900010215     C                   ENDIF
132000010215     C                   ENDIF
132100010215      *
132200041014     C   80              ADD       1             D59PAG
132300041014     C   80              write     TESTOR
132400050225     C   80              setoff                                       80
132500041013     C                   write     DETTOR
132600010220     C                   ADD       1             OTPTRI
132700010215      *
132800010215     C                   ENDDO
132900010215      *
133000010306     C     OTPTRI        IFGT      0
133100041014     C   80              ADD       1             D59PAG
133200041013     C   80              write     TESTOR
133300050225     C   80              setoff                                       80
133400080722     C                   write     TOTORM
133500010306     C                   ENDIF
133600010215      *
133700010215     C                   ENDSR
133800010215      *--
133900990921      *--------------------------------------------------------------*
134000990921      * Carico £6                                                    *
134100990921      *--------------------------------------------------------------*
134200920909     C     CARL6         BEGSR
134300020503     c                   clear                   trul06ds
134400020503     c                   eval      d06cod = '£6'
134500020503     c                   movel     d55fil        d06key
134600020503     c                   movel     trul06ds      kpjbu
134700020503     c                   call      'TRUL06R'
134800020503     c                   parm                    kpjba
134900020503     c                   movel     kpjbu         trul06ds
135000020503     c                   movea     lin£          L6
135100920909     C                   ENDSR
135200941110      *--------------------------------------------------------------*
135300941110      * Carico tabella codici bolla                                  *
135400941110      *--------------------------------------------------------------*
135500941110     C     CARTAB        BEGSR
135600941110     C*
135700990921     C* Reperisco moneta corrente
135800990921     C                   MOVEL     'GED'         KCOD
135900990921     C                   MOVEL     *BLANKS       KKEY
136000990921     C                   MOVEL     '1'           KKEY
136100990921     C     KTBE          CHAIN     TNTBE01L                           13
136200990921     C     *IN13         IFEQ      '0'
136300990921     C                   MOVEL     TBEUNI        DGED
136400990921     C                   END
136500030822     C*                  MOVEL     'ITL'         §GEDCR
136600990921     C*
136700941110     C                   MOVEL     '3A'          TBLCOD
136800941110     C                   Z-ADD     0             X                 3 0
136900941110     C     KTAB1         CHAIN     TABEL00F                           13
137000941110     C     *IN13         DOWEQ     '0'
137100941110     C     X             ANDLT     100
137200941110     C                   ADD       1             X
137300941110     C                   MOVEL     TBLKEY        C3A(X)
137400941110     C                   MOVEL     TBLUNI        D3A(X)
137500941110     C     KTAB1         READE     TABEL00F                               13
137600941110     C                   END
137700941110     C*
137800941110     C                   MOVEL     'CV'          TBLCOD
137900941110     C     KTAB1         CHAIN     TABEL00F                           13
138000941110     C     *IN13         DOWEQ     '0'
138100990921     C                   MOVEL     TBLKEY        DIVCV             3
138200990921     C                   MOVEL     TBLUNI        DSCV
138300990921     C* Se divisa diversa dalla moneta di conto reperisco data
138400990921     C* validità x trovare altra divisa da utilizzare x incassi
138500011009     C     §GEDCR        IFEQ      DIVCV
138600990921     C                   MOVEL     §CVFDC        DECIM             1
138700990921     C                   ELSE
138800990921     C     DIVEUR        IFEQ      *BLANKS
138900990921     C     §CVDVA        ANDGE     'S'
139000020103     C                   MOVEL     DIVCV         DIVEUR            3
139100990927     C                   MOVEL     §CVFDC        DECEUR            1
139200990921     C                   END
139300990921     C                   END
139400941110     C     KTAB1         READE     TABEL00F                               13
139500950925     C                   END
139600000606     C*  Caricamento Tabella 2Z
139700000606     C                   Z-ADD     0             X                 3 0
139800000704     C                   MOVEL     '2Z'          TBLCOD
139900000606     C     KTAB1         CHAIN     TABEL00F                           96
140000000606     C     *IN96         DOWEQ     '0'
140100000606     C     X             ANDLT     100
140200000606     C     TBLFLG        IFEQ      ' '
140300000606     C                   ADD       1             X
140400000606     C                   MOVEL     TBLKEY        C2Z(X)
140500000606     C                   MOVEL     TBLUNI        D2Z(X)
140600000606     C                   END
140700000606     C     KTAB1         READE     TABEL00F                               96
140800000606     C                   END
140900941110     C                   ENDSR
141000941109      *--------------------------------------------------------------*
141100941109      * OPERAZIONI INIZIALI                                          *
141200941109      *--------------------------------------------------------------*
141300950531     C     DEFCAM        BEGSR
141400941109     C*
141500941109     C                   Z-ADD     1             CODUT
141600941109     C                   CALL      'X§PARUT'
141700941109     C                   PARM                    UT§DSE
141800941109     C                   MOVEL     RAGUT         RSUT             20
141900941109     C                   MOVEL     REC80         CNCR80
142000131011     C                   move      simpou        dutpou            3 0
142100941109     C*
142200941109     C                   TIME                    TIMES             6 0
142300960927     C                   TIME                    W0140            14 0
142400960927     C                   MOVE      W0140         UDATE8            8 0
142500050110     C                   MOVEL     UDATE8        COM04             4 0
142600050110     C                   MOVE      UDATE8        woggi
142700050110     C                   MOVEL     COM04         woggi
142800941109      *---------------------------------------------------------------*
142900021204     C     Kapd          KLIST
143000021204     C                   KFLD                    apdtip
143100021204     C                   KFLD                    arbpdc
143200021204     C*
143300021204     C     Kapdd         KLIST
143400021204     C                   KFLD                    apdtip
143500110520     C                   KFLD                    f_dstpdr
143600021204     c                   movel     'A'           apdtip
143700021204     C*
143800941109     C     KTAB          KLIST
143900941109     C                   KFLD                    CODUT
144000941109     C                   KFLD                    TBLCOD
144100941109     C                   KFLD                    TBLKEY
144200941110     C*
144300941110     C     KTAB1         KLIST
144400941110     C                   KFLD                    CODUT
144500941110     C                   KFLD                    TBLCOD
144600990921     C     KTBE          KLIST
144700990921     C                   KFLD                    KCOD
144800990921     C                   KFLD                    KKEY
144900941109     C*
145000941109     C     KARB1         KLIST
145100941109     C                   KFLD                    ARBAAS
145200941109     C                   KFLD                    ARBLNP
145300941109     C                   KFLD                    ARBNRS
145400941109     C                   KFLD                    ARBNSP
145500061120     C     KARB5         KLIST
145600061120     C                   KFLD                    ARBAAS
145700061120     C                   KFLD                    ARBLNP
145800061120     C                   KFLD                    ARBNRS
145900061120     C                   KFLD                    ARBNSP
146000061120     C                   KFLD                    AR5trd
146100941109     C     KFVV          KLIST
146200020916     C                   KFLD                    d55fil
146300941109     C                   KFLD                    §NPG
146400941109     C                   KFLD                    FVVNFV
146500020916     C     KFVV1         KLIST
146600020916     C                   KFLD                    d55fil
146700020916     C                   KFLD                    §NPG
146800080401     c     kfidst        klist
146900080401     c                   kfld                    d55fil
147000080401     c                   kfld                    §npg
147100080401     c                   kfld                    Kfindc
147200960702     C     KARB          KLIST
147300020916     C                   KFLD                    Kfgs
147400020916     C                   KFLD                    KNDC
147500960702     C                   KFLD                    KDDC
147600030723     C                   KFLD                    Knss
147700030723     C     KARB2         KLIST
147800030723     C                   KFLD                    Kfgs
147900030723     C                   KFLD                    KNDC
148000030723     C                   KFLD                    KDDC
148100950602     C*
148200941109     C                   Z-ADD     4             §NPG
148300000606     C                   Z-ADD     66            LIN2
148400020916     C                   Z-ADD     d55fil        kfgs
148500030723     C     *LIKE         DEFINE    arbnss        knss
148600030723     C     *LIKE         DEFINE    arbifp        kfgs
148700020916     C     *LIKE         DEFINE    FVVNPG        §NPG
148800990921     C     *LIKE         DEFINE    AR6TRC        KTRC
148900990921     C     *LIKE         DEFINE    AR6AAS        KAAS
149000990921     C     *LIKE         DEFINE    AR6LNP        KLNP
149100990921     C     *LIKE         DEFINE    AR6NRS        KNRS
149200990921     C     *LIKE         DEFINE    AR6NSP        KNSP
149300990921     C     *LIKE         DEFINE    ARITIP        KTIP
149400000516     C     *LIKE         DEFINE    AR4TRC        KTRCA
149500941109     C*
149600000516     C     KAR4          KLIST
149700000516     C                   KFLD                    ARBAAS
149800000516     C                   KFLD                    ARBLNP
149900000516     C                   KFLD                    ARBNRS
150000000516     C                   KFLD                    ARBNSP
150100000516     C                   KFLD                    KTRCA
150200000516     C                   MOVE      'A'           KTRCA
150300000516     C*
150400990921     C     KAR6          KLIST
150500990921     C                   KFLD                    KAAS
150600990921     C                   KFLD                    KLNP
150700990921     C                   KFLD                    KNRS
150800990921     C                   KFLD                    KNSP
150900990921     C                   KFLD                    KTRC
151000990921     C     KARI          KLIST
151100990921     C                   KFLD                    KAAS
151200990921     C                   KFLD                    KLNP
151300990921     C                   KFLD                    KNRS
151400990921     C                   KFLD                    KNSP
151500990921     C                   KFLD                    KTIP
151600941109     C     KLBL          KLIST
151700941109     C                   KFLD                    LBLAAP
151800941109     C                   KFLD                    LBLLPP
151900941109     C                   KFLD                    LBLNRP
152000941109     C                   KFLD                    LBLNSP
152100941109     C*
152200941109     C     KLBL1         KLIST
152300941109     C                   KFLD                    LBLAAN
152400941109     C                   KFLD                    LBLLPN
152500941109     C                   KFLD                    LBLNRN
152600941109     C                   KFLD                    LBLNSN
152700010215      *--
152800010215     C     KFORM         KLIST
152900010215     C                   KFLD                    FVVFGS
153000010215     C                   KFLD                    KNDIS
153100041013      *--
153200041013     C     Kqdt          KLIST
153300041013     C                   KFLD                    d55fil
153400041013     C                   KFLD                    qdtnfv
153500041013     C                   KFLD                    qdtddc
153600010215      *--
153700941109     C*
153800960702     C     *LIKE         DEFINE    ARBNDC        KNDC
153900960702     C     *LIKE         DEFINE    ARBDDC        KDDC
154000990921     C     *LIKE         DEFINE    TBECOD        KCOD
154100990921     C     *LIKE         DEFINE    TBEKE1        KKEY
154200941109     C*
154300941110     C* Carico £6 della filiale in gestione
154400941109     C                   EXSR      CARL6
154500941110     C*
154600941110     C* Carico tabella codici bolla + valute
154700941110     C                   EXSR      CARTAB
154800941109     C*
154900941109     C                   ENDSR
155000941109      *---------------------------------------------------------------*
155100030723     c     srazznss      begsr
155200030723      *---------------------------------------------------------------*
155300030723     c                   z-add     9999          knss
155400030723     c     karb          setgt     fnarb70l
155500030723     c                   do        *hival
155600030723     c     karb2         readpe    fnarb70l
155700030723     c                   if        %eof(fnarb70l)
155800030723     c                   leave
155900030723     c                   end
156000030723     c* una volta azzerato tutto esco
156100030723     c                   if        arbnss = 0
156200030723     c                   leave
156300030723     c                   end
156400030723     c                   z-add     0             arbnss
156500030723     c                   except    aggnss
156600030723     c                   enddo
156700030723     c                   z-add     0             knss
156800030723     C                   ENDSR
156900070612      *---------------------------------------------------------*
157000070612      *  Determinazione del volume da utilizzare
157100070612      *---------------------------------------------------------*
157200070612     C     CALVOL        BEGSR
157300070612     c* t=volume da fatturare
157400070612     C     ARBFVF        IFEQ      'T'
157500070612     C                   Z-ADD     ARBVLF        WVOL
157600070612     C                   ELSE
157700070612     C     ARBNCR        ifeq      ARBNCL
157800070612     C                   Z-ADD     ARBVLC        WVOL
157900070612     C                   ELSE
158000070612     C     ARBVLC        IFGE      ARBVLF
158100070612     C                   Z-ADD     ARBVLC        WVOL
158200070612     C                   ELSE
158300070612     C                   Z-ADD     ARBVLF        WVOL
158400070612     C                   ENDIF
158500070612     C                   ENDIF
158600070612     C                   ENDIF
158700070612     C*
158800070612     C                   ENDSR
158900070612      *---------------------------------------------------------*
159000070612      *  Determinazione del peso da utilizzare
159100070612      *---------------------------------------------------------*
159200070612     C     calPES        BEGSR
159300070612     C     arbncp        ifeq      arbncl
159400070612     C                   Z-ADD     arbPKC        wpes
159500070612     C                   ELSE
159600070612     C     arbPKC        ifge      arbPKF
159700070612     C                   Z-ADD     arbPKC        wpes
159800070612     C                   else
159900070612     C                   Z-ADD     arbPKF        wpes
160000070612     C                   end
160100070612     C                   ENDIF
160200070612     C                   ENDSR
160300010215      *---------------------------------------------------------------*
160400000606     OSYSPRT    E            TESTS2         0 01
160500000606     O                       STT(1)              66
160600000606     O                       STT(2)             132
160700131011     O                       simpou               3
160800131011     o                                            4 '/'
160900131011     O                       SIMFEL               7
161000000606     O                       UDATE8             120 '  /  /    '
161100000606     O                       PAGE1         Z    132
161200000606     O          E            TESTS2         0 02
161300000606     O                       KNSIF               10
161400000606     O                       KNMUS               21
161500000606     O                                           75 'ANOMALIE DISTINTA SDA'
161600000606     O                       TIMES              118 '  .  .  '
161700000606     O          E            TESTS2         0 03
161800000606     O                                              'AUTOTRASPORTATORE:'
161900000606     O                       STCPDR            +  1
162000000606     O                       STDPDR            +  1
162100000606     O                                         +  5 'N.'
162200020916     O                       d55fil            +  1
162300020916     O                                         +  0 '/'
162400020916     O                       STCNDC        Z   +  0
162500000606     O                                         +  2 'DEL'
162600000606     O                       STCDDC        8   +  1
162700000606     O                                         +  2 'TIPO PRESTAZIONE:'
162800000606     O                       TPPRE             +  1
162900000606     O          E            TESTS2         0 04
163000000606     O                       STT(03)             66
163100000606     O                       STT(04)            132
163200011009     O                       §GEDCR              94
163300000606     O          E            TESTS2         1 05
163400000606     O                       STT(05)             66
163500000606     O                       STT(06)            132
163600000606     O          E            DETTS2      1
163700000606     O                       STDANO               1
163800000606     O                       STDTSP               3
163900000606     O                       STCZNC               7
164000000606     O                       STCFLS              12
164100000606     O                       STCNRS        Z     15
164200000606     O              N95N98   STCNCD        Z     23
164300000606     O              N95N98                       24 '/'
164400000606     O              N95N98   STCNCA        Z     31
164500000606     O               98                          31 'SOLO INCASSO'
164600000606     O               94                          31 'CON.PARZIALE'
164700000606     O              N77N98   STCTCO        Z     37
164800000606     O              N77N98   STCPKF        3     46
164900000606     O              N77N98   STCVLF        3     53
165000000606     O                       STCDES              66
165100000606     O                       STCMIT              78
165200000606     O                       STCTPI              82
165300000606     O              N92      STCIMP        2     94
165400000606     O               92      STCIPD              94 '    .   ,  '
165500000606     O                       STCVAL             111
165600000606     O              N93      STCIPV             107 '    .   ,   '
165700000606     O               93      STCIVL        2    106
165800000606     O                       STCLNP             115
165900000606     O                       STCNRS        Z    118
166000000606     O                       STCNSP        Z    126
166100000606     O                       STCNGD        Z    130
166200041216     O*                      STCFIN             132
166300000606     O          E    44      DETTS2      0
166400000606     O              N98N77   STCPKF        3     46
166500000606     O              N98N77   STCVLF        3     53
166600000606     O                                         +  0 '*'
166700000606     O          E    44      DETTS2      0
166800000606     O                                           54 '*'
166900000606     O          E            DETTSA      1
167000000609     O                                           15 'COD.CONSEGNA'
167100000609     O                                         +  1 'ANOMALA NON ATTRIBUIBILE'
167200950602     O*--------------------------------------------------------------*
167300950602     O*   AGGIORNAMENTI                                              *
167400950602     O*--------------------------------------------------------------*
167500030723     OFNARB30   E            AGGNSS
167600030723     O                       ARBNSS
167700011107** STT
167800941110                                         ***  D I S T I N T A    D   01
167900941110 I   C O N S E G N A  ***        FNLR55R                 PAG.        02
168000960927A-S-ZONA P.O.    SEGNACOLLI      COLLI    PESO VOLUME DESTINATARIO   03
168100991006 MITTENTE    TPI IMPORTO XXX     IMPORTO  DIV  NR.SPEDIZIONE  GIRO   04
168200941109------------------------------------------------------------------   05
168300941109------------------------------------------------------------------   06
168400050117                  D I S T I N T A   S E N Z A   A N O M A L I E      07
168500050117D I S T I N T A   C O N   A N O M A L I E   !!!                      08
