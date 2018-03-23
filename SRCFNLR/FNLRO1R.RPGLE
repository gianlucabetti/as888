000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020205     h*
000300020205     FFICSO00T  O    E             DISK    usropn
000400060214     ffiar401l  if   e           k disk
000500150220     ffidst01l  if   e           k disk
000600020206     ffnarb05l  Uf   e           k disk
000700020205     ftabel00f  if   e           k disk
000800121024     fazorg01l  if   e           k disk
000900000607     D kpjba         e ds
001000020205     D DS5A          e ds
001100121024     D og143         e ds
001200020205     D DSBL49        e ds
001300021014     D TIBS55DS      E DS
001400020205     D cncr80        E DS
001500020205     D UT§DSE0F      E DS
001600020205     D DSCMZ         E DS
001700020205     D PARAM5          DS
001800020205     D  LEGFIL                 1     10
001900021014     D  MBRFIL                11     20
002000020205     d gg              s              3  0
002100020206     d data6           s              6  0
002200020205     d dateu           s              8  0
002300020206     D Wdata           S               D   DATFMT(*eur)
002400020206     D oggi            S               d
002500020206     D dataiso         S               d
002600150416     d dteur           s               d   datfmt(*eur)
002700150416     d dtiso           s               d   datfmt(*iso)
002800020205     d orar            s              6  0
002900020205     d aora            s              2  0
003000020205     D nrrec           S              7  0
003100020206     D w0140           S             14  0
003200020206     D w0080           S              8  0
003300020205     D comman          S             80
003400021014     D CMD1            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003500021014     D CMD2            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003600021014     D CMD3            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003700021014     d   §membro       ds
003800021014     d  §emme                  1      1    inz('M')
003900021014     d  §fle                   2      4s 0
004000021014     d  §sed                   5      7s 0 inz(046)
004100021014     d  §mbr                   2      7
004200150408      * ?------------------------------------------------------------------ */
004300150408      *  Invio E-mail
004400150408      * ?------------------------------------------------------------------ */
004500150408     d wrkEml          s            253
004600150408     d wrkEmcc         s            253
004700150408     d wrkMsg          s           5000
004800150408     d wrkOgg          s             44
004900150416     d wrkEsit         s              1
005000150408      *
005100150416     D lista           S            100    DIM(30)
005200150408     D vuoto           S            100
005300150408      * ?------------------------------------------------------------------ */
005400020205     c**********************************************************************
005500020205     c                   z-add     0             nrrec
005600020206     c                   z-add     0             setll             3 0
005700020206     c     setll         setll     fnarb05l
005800000607     c                   do        *hival
005900020205     c                   read      fnarb05l
006000000607     c                   if        %eof
006100000607     c                   leave
006200000607     c                   end
006300020205      * solo bolle con data consegna > 0 e incasso contrassegno = 'R'
006400020205     c                   if        arbdcm = 0 or arbicc <> 'R'
006500020205     c                   iter
006600020205     c                   end
006700121024     c* individuo netwrk per differenziare tempi di attesa se estero
006800121024     c     arblna        chain     azorg01l
006900121024     c* se linea arrivo DPD e rigeneraz. x contrassegni omessi errore
007000121024     c                   If        %Found(azorg01l) and  orgfva = *blanks
007100121024     c                   MOVEL     ORGDE3        OG143
007200121024     c                   else
007300121024     c                   clear                   og143
007400121024     c                   endif
007500060214     c* recupero data di rigenerazione bolla (record 9 FiAR400F)
007600020205     c                   movel     '9'           ar4trc
007700060214     c     karb          chain     fiar401l
007800060214     c                   if        %found(fiar401l)
007900020205     c                   movel     ar4not        dsbl49
008000020211     c                   if        §b4ds9 <> 'PER C/ASSEGNO OMESSO'
008100020211     c                   iter
008200020211     c                   end
008300020205     c                   else
008400020205     c                   iter
008500000607     c                   end
008600150420      *
008700020225     c* se trascorsi xx (§5agrc) gg mando la
008800020205     c* variazione per recuperare contrassegni in sede
008900020206     c                   move      §b4dt9        dataiso
009000020206     c     oggi          subdur    dataiso       gg:*d
009100121024     c                   if        §ogntw = 'EEX' or §ogntw = 'DPD' or
009200121024     c                             §ogntw = 'FED'
009300121024     c                   if        gg >= §5al2
009400121024     c                   exsr      srvariaz
009500121024     c                   end
009600121024     c                   else
009700121024     c                   if        gg >= §5agrc
009800121024     c                   exsr      srvariaz
009900121024     c                   end
010000121024     c                   endif
010100150420      *
010200020205     c                   enddo
010300150420      *
010400020205      * Elaboro record scritti
010500020205     C                   if        nrrec <> *zeros
010600020205     C                   exsr      TRASMI
010700020205     c                   end
010800150420      **
010900150420      ** se ha caricato la schiera
011000150420      **   invia la mail
011100150420     C                   if        lista(1) <> *blank
011200150420     c                   exsr      invia_mail
011300150420     c                   end
011400000607
011500000607     c                   seton                                        lr
011600000607      *-----------------------------------------------
011700020205     c     srvariaz      begsr
011800000607      *-----------------------------------------------
011900150420      ****
012000150420     c* se la spedizione è in una distinta aperta non passo in sede
012100150420      **** DEVE SALTARE AL RECORD SUCCESSIVO dopo aver inserito in
012200150420     c* skiera per emissione mail successiva.
012300150420     c                   if        arbndc <> 0
012400150420     c                   z-add     arbifp        dstfgs
012500150420     c                   z-add     arbndc        dstnfv
012600150420     c     kdst          chain     fidst01l
012700150420     c                   if        %found(fidst01l) and dstfcf = ' '
012800150420      ***
012900150420      ***  carica in schiera per invio mail finale
013000150420     c                   exsr      sped_saltata
013100150420      ***
013200150420     c                   LEAVEsr
013300150420     c                   end
013400150420     c                   end
013500150420      ***
013600020205     C* scrivo record variazione per sede
013700020205     C                   clear                   FICSO000
013800020205     C                   MOVEL     'CRSE'        csoCAV
013900020205     C                   Z-ADD     dateu         csoDTV
014000020205     C                   TIME                    csoORV
014100020205     C                   Z-ADD     arblnp        csoLNP
014200020205     C                   Z-ADD     arblna        csoLNA
014300020205     C                   Z-ADD     arblna        csoFEV
014400020205     C                   Z-ADD     arbaas        csoAAS
014500020205     C                   Z-ADD     arbnrs        csoNRS
014600020205     C                   Z-ADD     arbnsp        csoNSP
014700020205     C                   MOVEL     '8'           csoSTA
014800020205     c                   write     FICSO000                             59
014900020205     c                   if        not *in59
015000020205     C                   add       1             nrrec
015100020205     c* aggioro file FNARB00f per poter poi far pulire la bolla
015200020205     C                   MOVEL     'Y'           arbicc
015300020205     c                   if        arbnmi = 0
015400020205     C                   MOVE      979797        arbnmi
015500020205     C                   MOVE      dateu         arbdmi
015600040830     C                   MOVE      dateu         arbdt1
015700040830     C                   MOVEl     'T'           arbft1
015800020205     c                   end
015900020205     c                   except    aggio
016000020205     c                   end
016100150420      ***
016200000607     c                   endsr
016300020207     C**-------------------------------------------------------------**
016400020207     C** Trasmette FICSO00T
016500020207     C**-------------------------------------------------------------**
016600020207     C     trasmi        BEGSR
016700020205     C                   CLOSE     FICSO00T
016800020205     C* Imposto parametri per locali
016900021014     C                   MOVEl(p)  §membro       MBRFIL
017000021014     C                   eval      LEGFIL = 'F' + §mbr + '999'
017100021014     C*  Richiamo TIBS55R per sapere librerie da/a cui trasmettere
017200021014     C                   CLEAR                   TIBS55ds
017300020228     c                   movel     'L'           i50tla
017400020205     C                   MOVEL     KNSIF         I50PSI
017500020205     C                   z-add     046           I50APO
017600020205     C                   z-add     SIMFEL        I50PPO
017700021014     C                   CALL      'TIBS55R'
017800021014     C                   PARM                    TIBS55ds
017900020205     C* DATI TRASMISSIONE
018000020205     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
018100020205     C                   MOVE      NRREC         CMZNRR                          N.REC.
018200020212     C                   MOVE      'LRO2'        CMZAZN                          AZIONE
018300020205     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
018400020205     C                   MOVEL     'FICSO00T'    cmzfld
018500020205     C                   MOVEL     'FICSO00R'    cmzfla
018600021014     C                   MOVEL(p)  §membro       CMZMBD
018700021014     C                   MOVEL(p)  §membro       CMZMBa
018800021016     C                   MOVEL     O50PLg        CMZLBD
018900021014     C                   MOVEL     O50ALG        CMZLBA
019000020205     C                   MOVE      '0000'        CMZORI                          ORA
019100020205     C                   MOVE      '0000000'     CMZDTI                          DATA
019200020205     C* TRASMETTO IN NORMALE SE ORA ATTUALE <= ORA TABELLA
019300020228     C                   MOVE      'R'           CMZURG                          NORMALE
019400020205     C*
019500020205     C                   MOVE      'A'           CMZADD                          IN ADD
019600020205     C                   MOVE      'S'           CMZCLR                          CLEAR
019700020205     C                   MOVE      'S'           CMZFTR                          DA TRANS.
019800020205     C                   MOVE      *BLANK        CMZERR                          1= ERR.
019900020205     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
020000020205     C                   MOVEL     PARAM5        W236            236
020100020205     C**----------------------------------
020200020205     C** SOTTOMETTE TRASMISSIONE
020300020205     C**----------------------------------
020400021014     C                   CALL      'TIBS53C'
020500020205     C                   PARM                    O50LCL
020600020205     C                   PARM                    CMZDST
020700020205     C                   PARM                    CMZFLD
020800020205     C                   PARM                    CMZMBD
020900020205     C                   PARM                    CMZLBD
021000020205     C                   PARM                    CMZFLA
021100020205     C                   PARM                    CMZMBA
021200020205     C                   PARM                    CMZLBA
021300020205     C                   PARM                    CMZORI
021400020205     C                   PARM                    CMZDTI
021500020205     C                   PARM                    CMZURG
021600020205     C                   PARM                    CMZADD
021700020205     C                   PARM                    CMZCLR
021800020205     C                   PARM                    CMZFTR
021900020205     C                   PARM                    CMZERR
022000020205     C                   PARM                    CMZAZN
022100020205     C                   PARM                    CMZSYF
022200020205     C                   PARM                    CMZNRR
022300020205     C                   PARM                    CMZPRR
022400020205     C                   PARM                    W236
022500020205     C*
022600020205     C                   ENDSR
022700020205     C**-------------------------------------------------------------**
022800020205     C** QCAEXEC SUL MBR DI FICSO00T
022900020205     C**-------------------------------------------------------------**
023000020205     C     CAEX          BEGSR
023100020205     C** Controllo se esiste membro
023200021014     C                   Z-ADD     49            LUNG             15 5
023300021014     C                   MOVEa     CMD3          COMMAN
023400021014     c                   eval      %subst(comman:43:6) = §mbr
023500020205     C                   CALL      'QCMDEXC'                            34
023600020205     C                   PARM                    COMMAN
023700020205     C                   PARM                    LUNG
023800020205     C** Se non esiste eseguo addpfm x membro nuovo
023900020205     C     *IN34         IFEQ      '1'
024000021014     C                   MOVEa     CMD1          COMMAN
024100021014     c                   eval      %subst(comman:43:6) = §mbr
024200020205     C                   CALL      'QCMDEXC'
024300020205     C                   PARM                    COMMAN
024400020205     C                   PARM                    LUNG
024500020205     C                   END
024600020205     C** Esegue in ogni caso OVRDBF del membro desiderato
024700021014     C                   MOVEa     CMD2          COMMAN
024800021014     c                   eval      %subst(comman:43:6) = §mbr
024900020205     C                   CALL      'QCMDEXC'
025000020205     C                   PARM                    COMMAN
025100020205     C                   PARM                    LUNG
025200020205     C*
025300020205     C                   ENDSR
025400000607      *-----------------------------------------------
025500000607     c     *inzsr        begsr
025600000607      *-----------------------------------------------
025700000607     c
025800000607     c     *entry        plist
025900020205     c                   parm                    kpjba
026000020205     C                   Z-ADD     1             CODUT
026100020205     C                   CALL      'X§PARUT'
026200020205     C                   PARM                    UT§DSE0F
026300020205     C                   MOVEL     REC80         CNCR80
026400021014     C                   MOVE      simfel        §fle
026500000607
026600000607     c     karb          klist
026700020205     c                   kfld                    arbaas
026800020205     c                   kfld                    arblnp
026900020205     c                   kfld                    arbnrs
027000020205     c                   kfld                    arbnsp
027100020205     c                   kfld                    ar4trc
027200020207     c     kspe          klist
027300020207     c                   kfld                    arbaas
027400020207     c                   kfld                    arblnp
027500020207     c                   kfld                    arbnrs
027600020207     c                   kfld                    arbnsp
027700020205     c     ktab          klist
027800020205     c                   kfld                    tblkut
027900020205     c                   kfld                    tblcod
028000020205     c                   kfld                    tblkey
028100150220     c     kdst          klist
028200150220     c                   kfld                    dstnpg
028300150220     c                   kfld                    dstnfv
028400150220     c                   kfld                    dstfgs
028500150220     c                   z-add     4             dstnpg
028600000607
028700020206     c                   time                    w0140
028800020206     c                   move      w0140         w0080
028900020206     C     *eur          MOVEL     w0080         Wdata
029000020206     C     *iso          MOVEL     Wdata         dateu
029100020206     c                   move      dateu         oggi
029200020205     c* aggancio tabella 5A per numero gg tentato recupero contrassegno
029300020205     c                   move      1             tblkut
029400020205     c                   movel     '5A'          tblcod
029500020205     c                   movel(p)  '1'           tblkey
029600020205     c     ktab          chain     tabel00f
029700020205     c                   if        %found(tabel00f)
029800020205     c                   movel     tbluni        ds5a
029900020205     c                   else
030000020225     c                   Z-ADD     999           §5Agrc
030100121029     c                   Z-ADD     999           §5Al2
030200020205     c                   end
030300020205     c* preparo il file da trasmettere in sede
030400020205     c                   exsr      caex
030500020206     c                   open      ficso00t
030600000607     c                   endsr
030700150408      * ?------------------------------------------------------------------ */
030800150408      * ?     Input            Verifica cosa deve segnalare
030900150408      * ?------------------------------------------------------------------ */
031000150416     C     sped_saltata  BEGSR                                                  *
031100150416      *
031200150416     c                   move      arbDDC        dtiso
031300150416     c                   move      dtiso         dteur
031400150416     c                   move      dteur         DDCgma            8 0
031500150416      *
031600150416     c                   z-add     1             xy                3 0
031700150408     c     vuoto         lookup    lista(xy)                              99
031800150416     c   99              eval      lista(xy) =    'Sped.: ' +
031900150416     c                             %editc(arbAAS:'X') + ' ' +
032000150416     c                             %editc(arbLNP:'X') + ' ' +
032100150416     c                             %editc(arbNRS:'X') + ' ' +
032200150416     c                             %editc(arbNSP:'X') + ' ' +
032300150416     c                             'Nr.Dist.: '             +
032400150416     c                             %editc(arbNDC:'Z') + ' ' +
032500150416     c                             'del '                   +
032600150416     c                             %editc(DDCgma:'Y')
032700150416     c  n99              clear                   lista(30)
032800150416     c  n99              eval      lista(30) = 'Presenti altri C/assegni non ' +
032900150416     c                             'inviati'
033000150416      *
033100150408     C                   endsr
033200150408      *____________________________________________________________________
033300150408      *   INVIA MAIL
033400150408      *____________________________________________________________________
033500150408     c     INVIA_MAIL    BEGSR
033600150408      *
033700150408     C* Inizializzo variabili
033800150417     C                   eval      wrkEml= 'Contrassegni@brt.it'
033900150416      **
034000150416     C                   eval      wrkOgg='C/Ass OMESSI non passati in gestio-
034100150416     C                             ne a sede'
034200150416      **
034300150416     C                   eval      wrkMsg='Attenzione i seguenti C/ASS OMES' +
034400150416     C                             'SI non sono stati passati in gestione a '+
034500150416     C                             'sede perché presenti in distinte Autotra'+
034600150416     C                             'sportatori NON ancora Contabilizzate.'
034700150416     c                                      +':/N'+
034800150416     C                             'Questa la lista delle spedizioni:'
034900150416     c                                      +':/N'+
035000150408     C                             lista(1) +':/N'+
035100150408     C                             lista(2) +':/N'+
035200150408     C                             lista(3) +':/N'+
035300150408     C                             lista(4) +':/N'+
035400150408     C                             lista(5) +':/N'+
035500150408     C                             lista(6) +':/N'+
035600150408     C                             lista(7) +':/N'+
035700150408     C                             lista(8) +':/N'+
035800150408     C                             lista(9) +':/N'+
035900150408     C                             lista(10)
036000150416      *
036100150416     c                   if        lista(11) <> *blank
036200150416     C                   eval      wrkMsg=   %trim(wrkMsg)
036300150416     c                                       +':/N'+
036400150416     C                             lista(11) +':/N'+
036500150416     C                             lista(12) +':/N'+
036600150416     C                             lista(13) +':/N'+
036700150416     C                             lista(14) +':/N'+
036800150416     C                             lista(15) +':/N'+
036900150416     C                             lista(16) +':/N'+
037000150416     C                             lista(17) +':/N'+
037100150416     C                             lista(18) +':/N'+
037200150416     C                             lista(19) +':/N'+
037300150416     C                             lista(20)
037400150416     c                   end
037500150416     c                   if        lista(21) <> *blank
037600150416     C                   eval      wrkMsg=   %trim(wrkMsg)
037700150416     c                                       +':/N'+
037800150416     C                             lista(21) +':/N'+
037900150416     C                             lista(22) +':/N'+
038000150416     C                             lista(23) +':/N'+
038100150416     C                             lista(24) +':/N'+
038200150416     C                             lista(25) +':/N'+
038300150416     C                             lista(26) +':/N'+
038400150416     C                             lista(27) +':/N'+
038500150416     C                             lista(28) +':/N'+
038600150416     C                             lista(29) +':/N'+
038700150416     C                             lista(30)
038800150416     c                   end
038900150408     C*
039000150408     C                   call(e)   'TIS701C1'
039100150408     C                   parm                    wrkEml
039200150408     C                   parm                    wrkEmcc
039300150408     C                   parm                    wrkOgg
039400150408     C                   parm                    wrkMsg
039500150408     C                   parm                    wrkesit
039600150408      *
039700150408     c                   ENDSR
039800150408      * ?------------------------------------------------------------------ */
039900000607     O*------------------------------------------------------*
040000020205     Ofnarb000  e            aggio
040100020205     o                       arbicc
040200020205     O                       arbnmi
040300020205     O                       arbdmi
040400080215     O                       arbdt1
040500080215     O                       arbft1
040600020205**         CMD1
040700021014ADDPFM FILE(FICSO00T)                MBR(M000000)
040800020205**         CMD2
040900021014OVRDBF FILE(FICSO00T)                MBR(M000000)
041000020205**         CMD3
041100021014CHKOBJ  OBJ(FICSO00T) OBJTYPE(*FILE) MBR(M000000)
