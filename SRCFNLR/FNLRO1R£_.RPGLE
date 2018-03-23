000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020205     h*
000300020205     FFICSO00T  O    E             DISK    usropn
000400060214     ffiar401l  if   e           k disk
000500020206     ffnarb05l  Uf   e           k disk
000600020205     ftabel00f  if   e           k disk
000601121024     fazorg01l  if   e           k disk
000700000607     D kpjba         e ds
000800020205     D DS5A          e ds
000801121024     D og143         e ds
000900020205     D DSBL49        e ds
001000021014     D TIBS55DS      E DS
001100020205     D cncr80        E DS
001200020205     D UT§DSE0F      E DS
001300020205     D DSCMZ         E DS
001400020205     D PARAM5          DS
001500020205     D  LEGFIL                 1     10
001600021014     D  MBRFIL                11     20
001700020205     d gg              s              3  0
001800020206     d data6           s              6  0
001900020205     d dateu           s              8  0
002000020206     D Wdata           S               D   DATFMT(*eur)
002100020206     D oggi            S               d
002200020206     D dataiso         S               d
002300020205     d orar            s              6  0
002400020205     d aora            s              2  0
002500020205     D nrrec           S              7  0
002600020206     D w0140           S             14  0
002700020206     D w0080           S              8  0
002800020205     D comman          S             80
002900021014     D CMD1            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003000021014     D CMD2            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003100021014     D CMD3            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003200021014     d   §membro       ds
003300021014     d  §emme                  1      1    inz('M')
003400021014     d  §fle                   2      4s 0
003500021014     d  §sed                   5      7s 0 inz(046)
003600021014     d  §mbr                   2      7
003700020205     c**********************************************************************
003800020205     c                   z-add     0             nrrec
003900020206     c                   z-add     0             setll             3 0
004000020206     c     setll         setll     fnarb05l
004100000607     c                   do        *hival
004200020205     c                   read      fnarb05l
004300000607     c                   if        %eof
004400000607     c                   leave
004500000607     c                   end
004600020205      * solo bolle con data consegna > 0 e incasso contrassegno = 'R'
004700020205     c                   if        arbdcm = 0 or arbicc <> 'R'
004800020205     c                   iter
004900020205     c                   end
004901121024     c* individuo netwrk per differenziare tempi di attesa se estero
004902121024     c     arblna        chain     azorg01l
004907121024     c* se linea arrivo DPD e rigeneraz. x contrassegni omessi errore
004908121024     c                   If        %Found(azorg01l) and  orgfva = *blanks
004910121024     c                   MOVEL     ORGDE3        OG143
004911121024     c                   else
004912121024     c                   clear                   og143
004913121024     c                   endif
005000060214     c* recupero data di rigenerazione bolla (record 9 FiAR400F)
005100020205     c                   movel     '9'           ar4trc
005200060214     c     karb          chain     fiar401l
005300060214     c                   if        %found(fiar401l)
005400020205     c                   movel     ar4not        dsbl49
005500020211     c                   if        §b4ds9 <> 'PER C/ASSEGNO OMESSO'
005600020211     c                   iter
005700020211     c                   end
005800020205     c                   else
005900020205     c                   iter
006000000607     c                   end
006100020225     c* se trascorsi xx (§5agrc) gg mando la
006200020205     c* variazione per recuperare contrassegni in sede
006300020206     c                   move      §b4dt9        dataiso
006400020206     c     oggi          subdur    dataiso       gg:*d
006401121024     c                   if        §ogntw = 'EEX' or §ogntw = 'DPD' or
006402121024     c                             §ogntw = 'FED'
006403121024     c                   if        gg >= §5al2
006404121024     c                   exsr      srvariaz
006405121024     c                   end
006701121024     c                   else
006702121024     c                   if        gg >= §5agrc
006703121024     c                   exsr      srvariaz
006704121024     c                   end
006708121024     c                   endif
006800020205     c                   enddo
006900020205      * Elaboro record scritti
007000020205     C                   if        nrrec <> *zeros
007100020205     C                   exsr      TRASMI
007200020205     c                   end
007300000607
007400000607     c                   seton                                        lr
007500000607      *-----------------------------------------------
007600020205     c     srvariaz      begsr
007700000607      *-----------------------------------------------
007800020205     C* scrivo record variazione per sede
007900020205     C                   clear                   FICSO000
008000020205     C                   MOVEL     'CRSE'        csoCAV
008100020205     C                   Z-ADD     dateu         csoDTV
008200020205     C                   TIME                    csoORV
008300020205     C                   Z-ADD     arblnp        csoLNP
008400020205     C                   Z-ADD     arblna        csoLNA
008500020205     C                   Z-ADD     arblna        csoFEV
008600020205     C                   Z-ADD     arbaas        csoAAS
008700020205     C                   Z-ADD     arbnrs        csoNRS
008800020205     C                   Z-ADD     arbnsp        csoNSP
008900020205     C                   MOVEL     '8'           csoSTA
009000020205     c                   write     FICSO000                             59
009100020205     c                   if        not *in59
009200020205     C                   add       1             nrrec
009300020205     c* aggioro file FNARB00f per poter poi far pulire la bolla
009400020205     C                   MOVEL     'Y'           arbicc
009500020205     c                   if        arbnmi = 0
009600020205     C                   MOVE      979797        arbnmi
009700020205     C                   MOVE      dateu         arbdmi
009800040830     C                   MOVE      dateu         arbdt1
009900040830     C                   MOVEl     'T'           arbft1
010000020205     c                   end
010100020205     c                   except    aggio
010200020205     c                   end
010300000607     c                   endsr
010400020207     C**-------------------------------------------------------------**
010500020207     C** Trasmette FICSO00T
010600020207     C**-------------------------------------------------------------**
010700020207     C     trasmi        BEGSR
010800020205     C                   CLOSE     FICSO00T
010900020205     C* Imposto parametri per locali
011000021014     C                   MOVEl(p)  §membro       MBRFIL
011100021014     C                   eval      LEGFIL = 'F' + §mbr + '999'
011200021014     C*  Richiamo TIBS55R per sapere librerie da/a cui trasmettere
011300021014     C                   CLEAR                   TIBS55ds
011400020228     c                   movel     'L'           i50tla
011500020205     C                   MOVEL     KNSIF         I50PSI
011600020205     C                   z-add     046           I50APO
011700020205     C                   z-add     SIMFEL        I50PPO
011800021014     C                   CALL      'TIBS55R'
011900021014     C                   PARM                    TIBS55ds
012000020205     C* DATI TRASMISSIONE
012100020205     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
012200020205     C                   MOVE      NRREC         CMZNRR                          N.REC.
012300020212     C                   MOVE      'LRO2'        CMZAZN                          AZIONE
012400020205     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
012500020205     C                   MOVEL     'FICSO00T'    cmzfld
012600020205     C                   MOVEL     'FICSO00R'    cmzfla
012700021014     C                   MOVEL(p)  §membro       CMZMBD
012800021014     C                   MOVEL(p)  §membro       CMZMBa
012900021016     C                   MOVEL     O50PLg        CMZLBD
013000021014     C                   MOVEL     O50ALG        CMZLBA
013100020205     C                   MOVE      '0000'        CMZORI                          ORA
013200020205     C                   MOVE      '0000000'     CMZDTI                          DATA
013300020205     C* TRASMETTO IN NORMALE SE ORA ATTUALE <= ORA TABELLA
013400020228     C                   MOVE      'R'           CMZURG                          NORMALE
013500020205     C*
013600020205     C                   MOVE      'A'           CMZADD                          IN ADD
013700020205     C                   MOVE      'S'           CMZCLR                          CLEAR
013800020205     C                   MOVE      'S'           CMZFTR                          DA TRANS.
013900020205     C                   MOVE      *BLANK        CMZERR                          1= ERR.
014000020205     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
014100020205     C                   MOVEL     PARAM5        W236            236
014200020205     C**----------------------------------
014300020205     C** SOTTOMETTE TRASMISSIONE
014400020205     C**----------------------------------
014500021014     C                   CALL      'TIBS53C'
014600020205     C                   PARM                    O50LCL
014700020205     C                   PARM                    CMZDST
014800020205     C                   PARM                    CMZFLD
014900020205     C                   PARM                    CMZMBD
015000020205     C                   PARM                    CMZLBD
015100020205     C                   PARM                    CMZFLA
015200020205     C                   PARM                    CMZMBA
015300020205     C                   PARM                    CMZLBA
015400020205     C                   PARM                    CMZORI
015500020205     C                   PARM                    CMZDTI
015600020205     C                   PARM                    CMZURG
015700020205     C                   PARM                    CMZADD
015800020205     C                   PARM                    CMZCLR
015900020205     C                   PARM                    CMZFTR
016000020205     C                   PARM                    CMZERR
016100020205     C                   PARM                    CMZAZN
016200020205     C                   PARM                    CMZSYF
016300020205     C                   PARM                    CMZNRR
016400020205     C                   PARM                    CMZPRR
016500020205     C                   PARM                    W236
016600020205     C*
016700020205     C                   ENDSR
016800020205     C**-------------------------------------------------------------**
016900020205     C** QCAEXEC SUL MBR DI FICSO00T
017000020205     C**-------------------------------------------------------------**
017100020205     C     CAEX          BEGSR
017200020205     C** Controllo se esiste membro
017300021014     C                   Z-ADD     49            LUNG             15 5
017400021014     C                   MOVEa     CMD3          COMMAN
017500021014     c                   eval      %subst(comman:43:6) = §mbr
017600020205     C                   CALL      'QCMDEXC'                            34
017700020205     C                   PARM                    COMMAN
017800020205     C                   PARM                    LUNG
017900020205     C** Se non esiste eseguo addpfm x membro nuovo
018000020205     C     *IN34         IFEQ      '1'
018100021014     C                   MOVEa     CMD1          COMMAN
018200021014     c                   eval      %subst(comman:43:6) = §mbr
018300020205     C                   CALL      'QCMDEXC'
018400020205     C                   PARM                    COMMAN
018500020205     C                   PARM                    LUNG
018600020205     C                   END
018700020205     C** Esegue in ogni caso OVRDBF del membro desiderato
018800021014     C                   MOVEa     CMD2          COMMAN
018900021014     c                   eval      %subst(comman:43:6) = §mbr
019000020205     C                   CALL      'QCMDEXC'
019100020205     C                   PARM                    COMMAN
019200020205     C                   PARM                    LUNG
019300020205     C*
019400020205     C                   ENDSR
019500000607      *-----------------------------------------------
019600000607     c     *inzsr        begsr
019700000607      *-----------------------------------------------
019800000607     c
019900000607     c     *entry        plist
020000020205     c                   parm                    kpjba
020100020205     C                   Z-ADD     1             CODUT
020200020205     C                   CALL      'X§PARUT'
020300020205     C                   PARM                    UT§DSE0F
020400020205     C                   MOVEL     REC80         CNCR80
020500021014     C                   MOVE      simfel        §fle
020600000607
020700000607     c     karb          klist
020800020205     c                   kfld                    arbaas
020900020205     c                   kfld                    arblnp
021000020205     c                   kfld                    arbnrs
021100020205     c                   kfld                    arbnsp
021200020205     c                   kfld                    ar4trc
021300020207     c     kspe          klist
021400020207     c                   kfld                    arbaas
021500020207     c                   kfld                    arblnp
021600020207     c                   kfld                    arbnrs
021700020207     c                   kfld                    arbnsp
021800020205     c     ktab          klist
021900020205     c                   kfld                    tblkut
022000020205     c                   kfld                    tblcod
022100020205     c                   kfld                    tblkey
022200000607
022300020206     c                   time                    w0140
022400020206     c                   move      w0140         w0080
022500020206     C     *eur          MOVEL     w0080         Wdata
022600020206     C     *iso          MOVEL     Wdata         dateu
022700020206     c                   move      dateu         oggi
022800020205     c* aggancio tabella 5A per numero gg tentato recupero contrassegno
022900020205     c                   move      1             tblkut
023000020205     c                   movel     '5A'          tblcod
023100020205     c                   movel(p)  '1'           tblkey
023200020205     c     ktab          chain     tabel00f
023300020205     c                   if        %found(tabel00f)
023400020205     c                   movel     tbluni        ds5a
023500020205     c                   else
023600020225     c                   Z-ADD     999           §5Agrc
023601121029     c                   Z-ADD     999           §5Al2
023700020205     c                   end
023800020205     c* preparo il file da trasmettere in sede
023900020205     c                   exsr      caex
024000020206     c                   open      ficso00t
024100000607     c                   endsr
024200000607     O*------------------------------------------------------*
024300020205     Ofnarb000  e            aggio
024400020205     o                       arbicc
024500020205     O                       arbnmi
024600020205     O                       arbdmi
024601080215     O                       arbdt1
024602080215     O                       arbft1
024700020205**         CMD1
024800021014ADDPFM FILE(FICSO00T)                MBR(M000000)
024900020205**         CMD2
025000021014OVRDBF FILE(FICSO00T)                MBR(M000000)
025100020205**         CMD3
025200021014CHKOBJ  OBJ(FICSO00T) OBJTYPE(*FILE) MBR(M000000)
