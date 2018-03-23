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
002600020205     d orar            s              6  0
002700020205     d aora            s              2  0
002800020205     D nrrec           S              7  0
002900020206     D w0140           S             14  0
003000020206     D w0080           S              8  0
003100020205     D comman          S             80
003200021014     D CMD1            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003300021014     D CMD2            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003400021014     D CMD3            S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
003500021014     d   §membro       ds
003600021014     d  §emme                  1      1    inz('M')
003700021014     d  §fle                   2      4s 0
003800021014     d  §sed                   5      7s 0 inz(046)
003900021014     d  §mbr                   2      7
004000150408      * ?------------------------------------------------------------------ */
004100150408      *  Invio E-mail
004200150408      * ?------------------------------------------------------------------ */
004300150408     d wrkEml          s            253
004400150408     d wrkEmcc         s            253
004500150408     d wrkMsg          s           5000
004600150408     d wrkOgg          s             44
004700150416     d wrkEsit         s              1
004800150408      *
004900150416     D lista           S            100    DIM(30)
005000150408     D vuoto           S            100
005100150408      * ?------------------------------------------------------------------ */
005200020205     c**********************************************************************
005300020205     c                   z-add     0             nrrec
005400020206     c                   z-add     0             setll             3 0
005500020206     c     setll         setll     fnarb05l
005600000607     c                   do        *hival
005700020205     c                   read      fnarb05l
005800000607     c                   if        %eof
005900000607     c                   leave
006000000607     c                   end
006100020205      * solo bolle con data consegna > 0 e incasso contrassegno = 'R'
006200020205     c                   if        arbdcm = 0 or arbicc <> 'R'
006300020205     c                   iter
006400020205     c                   end
006500121024     c* individuo netwrk per differenziare tempi di attesa se estero
006600121024     c     arblna        chain     azorg01l
006700121024     c* se linea arrivo DPD e rigeneraz. x contrassegni omessi errore
006800121024     c                   If        %Found(azorg01l) and  orgfva = *blanks
006900121024     c                   MOVEL     ORGDE3        OG143
007000121024     c                   else
007100121024     c                   clear                   og143
007200121024     c                   endif
007300060214     c* recupero data di rigenerazione bolla (record 9 FiAR400F)
007400020205     c                   movel     '9'           ar4trc
007500060214     c     karb          chain     fiar401l
007600060214     c                   if        %found(fiar401l)
007700020205     c                   movel     ar4not        dsbl49
007800020211     c                   if        §b4ds9 <> 'PER C/ASSEGNO OMESSO'
007900020211     c                   iter
008000020211     c                   end
008100020205     c                   else
008200020205     c                   iter
008300000607     c                   end
008400150220     c* se la spedizione è in una distinta aperta non passo in sede
008500150220     c                   if        arbndc <> 0
008600150220     c                   z-add     arbifp        dstfgs
008700150220     c                   z-add     arbndc        dstnfv
008800150220     c     kdst          chain     fidst01l
008900150220     c                   if        %found(fidst01l) and dstfcf = ' '
009000150416      ***
009100150416      ***  carica in schiera per invio mail finale
009200150416     c                   exsr      sped_saltata
009300150416      ***
009400150220     c                   iter
009500150220     c                   end
009600150220     c                   end
009700020225     c* se trascorsi xx (§5agrc) gg mando la
009800020205     c* variazione per recuperare contrassegni in sede
009900020206     c                   move      §b4dt9        dataiso
010000020206     c     oggi          subdur    dataiso       gg:*d
010100121024     c                   if        §ogntw = 'EEX' or §ogntw = 'DPD' or
010200121024     c                             §ogntw = 'FED'
010300121024     c                   if        gg >= §5al2
010400121024     c                   exsr      srvariaz
010500121024     c                   end
010600121024     c                   else
010700121024     c                   if        gg >= §5agrc
010800121024     c                   exsr      srvariaz
010900121024     c                   end
011000121024     c                   endif
011100020205     c                   enddo
011200020205      * Elaboro record scritti
011300020205     C                   if        nrrec <> *zeros
011400020205     C                   exsr      TRASMI
011500150416      ** se ha caricato la schiera
011600150416      **   invia la mail
011700150416     C                   if        lista(1) <> *blank
011800150416     c                   exsr      invia_mail
011900150416     c                   end
012000150416      **
012100020205     c                   end
012200000607
012300000607     c                   seton                                        lr
012400000607      *-----------------------------------------------
012500020205     c     srvariaz      begsr
012600000607      *-----------------------------------------------
012700020205     C* scrivo record variazione per sede
012800020205     C                   clear                   FICSO000
012900020205     C                   MOVEL     'CRSE'        csoCAV
013000020205     C                   Z-ADD     dateu         csoDTV
013100020205     C                   TIME                    csoORV
013200020205     C                   Z-ADD     arblnp        csoLNP
013300020205     C                   Z-ADD     arblna        csoLNA
013400020205     C                   Z-ADD     arblna        csoFEV
013500020205     C                   Z-ADD     arbaas        csoAAS
013600020205     C                   Z-ADD     arbnrs        csoNRS
013700020205     C                   Z-ADD     arbnsp        csoNSP
013800020205     C                   MOVEL     '8'           csoSTA
013900020205     c                   write     FICSO000                             59
014000020205     c                   if        not *in59
014100020205     C                   add       1             nrrec
014200020205     c* aggioro file FNARB00f per poter poi far pulire la bolla
014300020205     C                   MOVEL     'Y'           arbicc
014400020205     c                   if        arbnmi = 0
014500020205     C                   MOVE      979797        arbnmi
014600020205     C                   MOVE      dateu         arbdmi
014700040830     C                   MOVE      dateu         arbdt1
014800040830     C                   MOVEl     'T'           arbft1
014900020205     c                   end
015000020205     c                   except    aggio
015100020205     c                   end
015200000607     c                   endsr
015300020207     C**-------------------------------------------------------------**
015400020207     C** Trasmette FICSO00T
015500020207     C**-------------------------------------------------------------**
015600020207     C     trasmi        BEGSR
015700020205     C                   CLOSE     FICSO00T
015800020205     C* Imposto parametri per locali
015900021014     C                   MOVEl(p)  §membro       MBRFIL
016000021014     C                   eval      LEGFIL = 'F' + §mbr + '999'
016100021014     C*  Richiamo TIBS55R per sapere librerie da/a cui trasmettere
016200021014     C                   CLEAR                   TIBS55ds
016300020228     c                   movel     'L'           i50tla
016400020205     C                   MOVEL     KNSIF         I50PSI
016500020205     C                   z-add     046           I50APO
016600020205     C                   z-add     SIMFEL        I50PPO
016700021014     C                   CALL      'TIBS55R'
016800021014     C                   PARM                    TIBS55ds
016900020205     C* DATI TRASMISSIONE
017000020205     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
017100020205     C                   MOVE      NRREC         CMZNRR                          N.REC.
017200020212     C                   MOVE      'LRO2'        CMZAZN                          AZIONE
017300020205     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
017400020205     C                   MOVEL     'FICSO00T'    cmzfld
017500020205     C                   MOVEL     'FICSO00R'    cmzfla
017600021014     C                   MOVEL(p)  §membro       CMZMBD
017700021014     C                   MOVEL(p)  §membro       CMZMBa
017800021016     C                   MOVEL     O50PLg        CMZLBD
017900021014     C                   MOVEL     O50ALG        CMZLBA
018000020205     C                   MOVE      '0000'        CMZORI                          ORA
018100020205     C                   MOVE      '0000000'     CMZDTI                          DATA
018200020205     C* TRASMETTO IN NORMALE SE ORA ATTUALE <= ORA TABELLA
018300020228     C                   MOVE      'R'           CMZURG                          NORMALE
018400020205     C*
018500020205     C                   MOVE      'A'           CMZADD                          IN ADD
018600020205     C                   MOVE      'S'           CMZCLR                          CLEAR
018700020205     C                   MOVE      'S'           CMZFTR                          DA TRANS.
018800020205     C                   MOVE      *BLANK        CMZERR                          1= ERR.
018900020205     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
019000020205     C                   MOVEL     PARAM5        W236            236
019100020205     C**----------------------------------
019200020205     C** SOTTOMETTE TRASMISSIONE
019300020205     C**----------------------------------
019400021014     C                   CALL      'TIBS53C'
019500020205     C                   PARM                    O50LCL
019600020205     C                   PARM                    CMZDST
019700020205     C                   PARM                    CMZFLD
019800020205     C                   PARM                    CMZMBD
019900020205     C                   PARM                    CMZLBD
020000020205     C                   PARM                    CMZFLA
020100020205     C                   PARM                    CMZMBA
020200020205     C                   PARM                    CMZLBA
020300020205     C                   PARM                    CMZORI
020400020205     C                   PARM                    CMZDTI
020500020205     C                   PARM                    CMZURG
020600020205     C                   PARM                    CMZADD
020700020205     C                   PARM                    CMZCLR
020800020205     C                   PARM                    CMZFTR
020900020205     C                   PARM                    CMZERR
021000020205     C                   PARM                    CMZAZN
021100020205     C                   PARM                    CMZSYF
021200020205     C                   PARM                    CMZNRR
021300020205     C                   PARM                    CMZPRR
021400020205     C                   PARM                    W236
021500020205     C*
021600020205     C                   ENDSR
021700020205     C**-------------------------------------------------------------**
021800020205     C** QCAEXEC SUL MBR DI FICSO00T
021900020205     C**-------------------------------------------------------------**
022000020205     C     CAEX          BEGSR
022100020205     C** Controllo se esiste membro
022200021014     C                   Z-ADD     49            LUNG             15 5
022300021014     C                   MOVEa     CMD3          COMMAN
022400021014     c                   eval      %subst(comman:43:6) = §mbr
022500020205     C                   CALL      'QCMDEXC'                            34
022600020205     C                   PARM                    COMMAN
022700020205     C                   PARM                    LUNG
022800020205     C** Se non esiste eseguo addpfm x membro nuovo
022900020205     C     *IN34         IFEQ      '1'
023000021014     C                   MOVEa     CMD1          COMMAN
023100021014     c                   eval      %subst(comman:43:6) = §mbr
023200020205     C                   CALL      'QCMDEXC'
023300020205     C                   PARM                    COMMAN
023400020205     C                   PARM                    LUNG
023500020205     C                   END
023600020205     C** Esegue in ogni caso OVRDBF del membro desiderato
023700021014     C                   MOVEa     CMD2          COMMAN
023800021014     c                   eval      %subst(comman:43:6) = §mbr
023900020205     C                   CALL      'QCMDEXC'
024000020205     C                   PARM                    COMMAN
024100020205     C                   PARM                    LUNG
024200020205     C*
024300020205     C                   ENDSR
024400000607      *-----------------------------------------------
024500000607     c     *inzsr        begsr
024600000607      *-----------------------------------------------
024700000607     c
024800000607     c     *entry        plist
024900020205     c                   parm                    kpjba
025000020205     C                   Z-ADD     1             CODUT
025100020205     C                   CALL      'X§PARUT'
025200020205     C                   PARM                    UT§DSE0F
025300020205     C                   MOVEL     REC80         CNCR80
025400021014     C                   MOVE      simfel        §fle
025500000607
025600000607     c     karb          klist
025700020205     c                   kfld                    arbaas
025800020205     c                   kfld                    arblnp
025900020205     c                   kfld                    arbnrs
026000020205     c                   kfld                    arbnsp
026100020205     c                   kfld                    ar4trc
026200020207     c     kspe          klist
026300020207     c                   kfld                    arbaas
026400020207     c                   kfld                    arblnp
026500020207     c                   kfld                    arbnrs
026600020207     c                   kfld                    arbnsp
026700020205     c     ktab          klist
026800020205     c                   kfld                    tblkut
026900020205     c                   kfld                    tblcod
027000020205     c                   kfld                    tblkey
027100150220     c     kdst          klist
027200150220     c                   kfld                    dstnpg
027300150220     c                   kfld                    dstnfv
027400150220     c                   kfld                    dstfgs
027500150220     c                   z-add     4             dstnpg
027600000607
027700020206     c                   time                    w0140
027800020206     c                   move      w0140         w0080
027900020206     C     *eur          MOVEL     w0080         Wdata
028000020206     C     *iso          MOVEL     Wdata         dateu
028100020206     c                   move      dateu         oggi
028200020205     c* aggancio tabella 5A per numero gg tentato recupero contrassegno
028300020205     c                   move      1             tblkut
028400020205     c                   movel     '5A'          tblcod
028500020205     c                   movel(p)  '1'           tblkey
028600020205     c     ktab          chain     tabel00f
028700020205     c                   if        %found(tabel00f)
028800020205     c                   movel     tbluni        ds5a
028900020205     c                   else
029000020225     c                   Z-ADD     999           §5Agrc
029100121029     c                   Z-ADD     999           §5Al2
029200020205     c                   end
029300020205     c* preparo il file da trasmettere in sede
029400020205     c                   exsr      caex
029500020206     c                   open      ficso00t
029600000607     c                   endsr
029700150408      * ?------------------------------------------------------------------ */
029800150408      * ?     Input            Verifica cosa deve segnalare
029900150408      * ?------------------------------------------------------------------ */
030000150416     C     sped_saltata  BEGSR                                                  *
030100150416      *
030200150416     c                   z-add     1             xy                3 0
030300150408     c     vuoto         lookup    lista(xy)                              99
030400150416     c   99              eval      lista(xy) =    'Sped.: ' +
030500150416     c                             %editc(arbAAS:'X') + ' ' +
030600150416     c                             %editc(arbLNP:'X') + ' ' +
030700150416     c                             %editc(arbNRS:'X') + ' ' +
030800150416     c                             %editc(arbNSP:'X') + ' ' +
030900150416     c                             'Nr.Dist.: '             +
031000150416     c                             %editc(arbNDC:'Z') + ' ' +
031100150416     c                             'del '                   +
031200150416     c                             %editw(arbDDC:'    /  /  ')
031300150416     c  n99              clear                   lista(30)
031400150416     c  n99              eval      lista(30) = 'Presenti altri C/assegni non ' +
031500150416     c                             'inviati'
031600150416      *
031700150408     C                   endsr
031800150408      *____________________________________________________________________
031900150408      *   INVIA MAIL
032000150408      *____________________________________________________________________
032100150408     c     INVIA_MAIL    BEGSR
032200150408      *
032300150408     C* Inizializzo variabili
032400150408     C                   eval      wrkEml= 'gestione.clienti@brt.it'
032500150416      **
032600150416     C                   eval      wrkOgg='C/ASS non inviati.  '+
032700150416     C                             '(Filiale ' + %EDITC(§Fle:'X') +')'
032800150416      **
032900150416     C                   eval      wrkMsg='Attenzione i seguenti C/ASS non ' +
033000150416     C                             'sono stati inviati perché la distinta '  +
033100150416     C                             'è ancora aperta. '
033200150416     c                                      +':/N'+
033300150416     C                             'Questa la lista delle spedizioni:'
033400150416     c                                      +':/N'+
033500150408     C                             lista(1) +':/N'+
033600150408     C                             lista(2) +':/N'+
033700150408     C                             lista(3) +':/N'+
033800150408     C                             lista(4) +':/N'+
033900150408     C                             lista(5) +':/N'+
034000150408     C                             lista(6) +':/N'+
034100150408     C                             lista(7) +':/N'+
034200150408     C                             lista(8) +':/N'+
034300150408     C                             lista(9) +':/N'+
034400150408     C                             lista(10)
034500150416      *
034600150416     c                   if        lista(11) <> *blank
034700150416     C                   eval      wrkMsg=   %trim(wrkMsg)
034800150416     c                                       +':/N'+
034900150416     C                             lista(11) +':/N'+
035000150416     C                             lista(12) +':/N'+
035100150416     C                             lista(13) +':/N'+
035200150416     C                             lista(14) +':/N'+
035300150416     C                             lista(15) +':/N'+
035400150416     C                             lista(16) +':/N'+
035500150416     C                             lista(17) +':/N'+
035600150416     C                             lista(18) +':/N'+
035700150416     C                             lista(19) +':/N'+
035800150416     C                             lista(20)
035900150416     c                   end
036000150416     c                   if        lista(21) <> *blank
036100150416     C                   eval      wrkMsg=   %trim(wrkMsg)
036200150416     c                                       +':/N'+
036300150416     C                             lista(21) +':/N'+
036400150416     C                             lista(22) +':/N'+
036500150416     C                             lista(23) +':/N'+
036600150416     C                             lista(24) +':/N'+
036700150416     C                             lista(25) +':/N'+
036800150416     C                             lista(26) +':/N'+
036900150416     C                             lista(27) +':/N'+
037000150416     C                             lista(28) +':/N'+
037100150416     C                             lista(29) +':/N'+
037200150416     C                             lista(30)
037300150416     c                   end
037400150408     C*
037500150408     C                   call(e)   'TIS701C1'
037600150408     C                   parm                    wrkEml
037700150408     C                   parm                    wrkEmcc
037800150408     C                   parm                    wrkOgg
037900150408     C                   parm                    wrkMsg
038000150408     C                   parm                    wrkesit
038100150408      *
038200150408     c                   ENDSR
038300150408      * ?------------------------------------------------------------------ */
038400000607     O*------------------------------------------------------*
038500020205     Ofnarb000  e            aggio
038600020205     o                       arbicc
038700020205     O                       arbnmi
038800020205     O                       arbdmi
038900080215     O                       arbdt1
039000080215     O                       arbft1
039100020205**         CMD1
039200021014ADDPFM FILE(FICSO00T)                MBR(M000000)
039300020205**         CMD2
039400021014OVRDBF FILE(FICSO00T)                MBR(M000000)
039500020205**         CMD3
039600021014CHKOBJ  OBJ(FICSO00T) OBJTYPE(*FILE) MBR(M000000)
