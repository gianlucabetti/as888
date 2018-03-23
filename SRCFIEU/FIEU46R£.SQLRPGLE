000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200050715      * Fieu46R *-----------------------------------------------------------*
000300050715      *                                                                     *
000400050715      * In Base all'ordine DPD controlla se è possibile eliminare           *
000500050715      * un C.Request inviato precedentemente. Se però l'ORM è stato         *
000600050715      * inserito occorre proporre una variazione a STORNO e se              *
000700050715      * l'ORM è diventato una Bolla non si deve fare + Nulla.               *
000800050705      **?------------------------------------------------------------------ */
000900050928     Ffidpo03l  uf   E           k DISK
001000050722     Ffnorp00f  o  a E             DISK
001100061005     Ffnorm07l  if   E           k DISK
001200050922     FAZorg01l  if   E           k DISK
001300061005     Ffiar403l  if   E           k disk
001400061005      *
001500061018     d fieu48ds      e ds
001600050922     D OG143         E DS
001700050922     D XX              S              3  0 INZ
001800050922     D FIL             S              3S 0 DIM(50) descend
001900060811     D rifalfa15       S             15a   INZ
002000050722      *
002100050722     D  data_Oggi      s              8  0 INZ(0)
002200050722     D  day            s              8  0 INZ(0)
002300050722     D  ore            s              6  0 INZ(0)
002400050722     D  w0140          s             14  0 INZ(0)
002500050722     D*
002600050722     D WLBDAT          DS
002700050722     D  G02DAT                 1      8  0
002800050722     D  G02INV                 9     16  0
002900050722     D  G02ERR                17     17
003000050722     D  G02TGI                18     22  0
003100050722      *
003200070612     D dsbl4M        E DS
003300060908     D KPJBA         E DS
003400060908     D trul33ds      E DS
003500050922     D  LNPOE          s              3  0 INZ(0)
003600050922     D  LNPOC          s              3  0 INZ(0)
003700061005
003800061005     D alfa10          s             10
003900061005     D alfa15          s             15
004000050704      **?------------------------------------------------------------------ */
004100050704      *    main
004200050704      **?------------------------------------------------------------------ */
004300050715     c     *entry        plist
004400060809     C                   parm                    Key_DPDORM       10
004500050722      *
004600050722      *  imposta UDATE di elaborazione
004700050722     C                   move      *date         G02DAT
004800050722     C                   MOVE      *ZEROS        G02INV
004900050722     C                   MOVE      *BLANKS       G02ERR
005000050722     C                   CALL      'XSRDA8'
005100050722     C                   PARM                    WLBDAT
005200050722     C                   Z-ADD     G02INV        data_oggi
005300050722     C                   move      data_oggi     day
005400050722     C                   TIME                    W0140
005500050722     C                   MOVEl     W0140         ore
005600050722      *
005700060809     C                   movel     Key_DPDORM    ODEPOT            4
005800050715     C                   move      Key_DPDORM    ORDERNR           6 0
005900060811     C                   eval      rifalfa15 = 'DPD'+ %trim(Key_DPDORM)
006000050922      *
006100050922      * carico tutte le HUB / Filiali
006200050922     c                   clear                   xx
006300050922     C     *loval        setll     azorg01l
006400050922     C                   read      azorg01l
006500050922      *
006600061019     C                   DOW       NOT %EOF(azorg01l)
006700061019     C                   if        ORGfva = *blanks
006800050922     c                             and (orgfag='A' or orgfag='F')
006900050922      *
007000050922     C                   movel     ORGDE3        OG143
007100050922     c* solo per tipi F o a
007200050922     C                   IF        §ogNTW = 'DPD'
007300050922     C                   z-add     1             XX
007400050922     c     orgfil        lookup    FIL(xx)                                31
007500050922     c                   if        not   *in31
007600050922     C                   z-add     1             XX
007700050922     c     *zeros        lookup    FIL(xx)                                31
007800050922     c   31              movel     orgfil        FIL(XX)
007900050922     c                   end
008000050922     C                   ENDIF
008100050922      *
008200061019     c                   end
008300050922     C                   read      azorg01l
008400050922     C                   ENDDO
008500050922      *------------------
008600061005      *
008700061005     C     Kar43         KLIST
008800061005     C                   KFLD                    ar4trc
008900061005     C                   KFLD                    ar4n30
009000050715
009100050715     c     OrdineDPD     klist
009200050715     C                   kfld                    ODEPOT
009300050715     C                   kfld                    ORDERNR
009400050715
009500050715     c     ORP_Key       klist
009600050715     C                   kfld                    DPOPOE
009700050715     C                   kfld                    DPONRS
009800050715     C                   kfld                    DPONOR
009900050715     C                   kfld                    DPONRV
010000050715
010100050715      **?Esegue un controllo per vedere a che punto è l'ORM in Bartolini    */
010200050715      **?e quindi dove intervenire:                                         */
010300050928     c     OrdineDPD     chain     FIDPO03L
010400050715
010500060908     c                   if        %Found(FIDPO03L)
010600060908
010700060908      ** in questo caso occorre vedere se è possibile bloccare l'ORM creato
010800060908      ** eseguendo una variazione di ORM
010900050715
011000050718      **? Fa una proposta di variazione ORM
011100050722      **?   impostando la causale "80"
011200061005      * prepara la chiave per agganciare l'orm
011300061005     c                   clear                   alfa10
011400061005     c                   movel     dpodep        alfa10
011500061005     c                   move      dpoord        alfa10
011600061005     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
011700061005
011800061005      * aggancia l'orm Bartolini
011900061005     c     alfa15        chain     fnorm07l
012000061005      **?           ---------------
012100061005     c                   if        %Found(fnORM07l)
012200061005      **?           ---------------
012300061005
012400060907      ** Se l'ORM è già stato eseguito occorre informare DPD con lo STATUS 5
012500060907      **  Cancellation not possible.
012600060908      **?   poichè già bollettato
012700061005     c                   If        ormFAO >= 600
012800061005
012900060914      *?      ATTENZIONE: cancellazione non possibile invio STATUS --> 5
013000061005      *?                        impostando il flag a 15 x il FIEU41R
013100060920     c                   z-add     15            dpoSTS
013200061005
013300061005      *?   poi comunque si controlla che, se già chiuso l'ORM Bartolini non vi siano
013400061005      *?    legate delle bolle quindi si da l'OK sull'annullamento.
013500061005     c                   if        ormFAO = 999
013600061005
013700061005     c                   eval      ar4trc ='M'
013800061005     c                   move      dpoPOE        dpoPOEa           3
013900061005     c                   move      dpoNRS        dpoNRSa           2
014000061005     c                   move      dpoNOR        dpoNORa           7
014100061005     c                   move      dpoNRV        dpoNRVa           2
014200070612     c*******            eval      ar4n30 = %trim(dpopoeA) + %trim(dponrsA) +
014300070612     c*******                               %trim(dponorA) + %trim(dponrvA)
014400070612     c                   move      dpopoeA       §B4POE
014500070612     c                   move      dponrsA       §B4NSR
014600070612     c                   move      dponorA       §B4NOR
014700070612     c                   move      dponrvA       §B4NRV
014800070612     c                   eval      ar4n30 = dsbl4m
014900061005      * controlla se non vi è legata alcuna bolla
015000061005     C     Kar43         chain     fiar403l
015100061005      **?   qui la fase è  già chiuso e senza una bolla
015200061005     c                   if        not %Found(fiar403l)
015300061005
015400061005      ** Se invece era stato chiuso per qualche motivo quindi la Fase è >=900
015500061005      ** Memorizzo con un codice di errore l'accaduto sul FIDPO.
015600061005      **  .....come STORNO mancato x chiusura ORM -->"07"
015700061213
015800061213     c*******  Da non visualizzare + come segnalazione poichè non è un errore
015900061213     c*******            eval      dpoCODER = '07'
016000061213     c                   eval      dpoCODER = *blank
016100061213
016200061005      *?      ATTENZIONE: se la cancellazione è stata fatta OK
016300061005      *?          invio STATUS --> 3     impostando il flag a 13 x il FIEU41R
016400061005     c                   z-add     13            dpoSTS
016500061005     c                   end
016600061005
016700061005     c                   endIF
016800061005
016900061005     c                   z-add     0             dpoDTR
017000061005     c                   update    fiDPO000
017100061005
017200060907     c                   ElsE
017300061005      **?   qui la fase è < di 600
017400060908      ** se
017500060908      **?   non ancora bollettato
017600060908      **            occorre fare una proposta di storno --> scrivo ORP
017700050722     c                   clear                   fnORP000
017800050722     c                   movel     ormPOE        orpPOE
017900050722     c                   movel     ormNSR        orpNSR
018000050722     c                   movel     ormNOR        orpNOR
018100050722     c                   movel     ormNRV        orpNRV
018200050722     c                   movel     ormTOR        orpTOR
018300050722     c                   movel     ormCOR        orpCOR
018400050722     c                   movel     ormRSO        orpRSO
018500050722     c                   movel     ormINO        orpINO
018600050722     c                   movel     ormCAO        orpCAO
018700050722     c                   movel     ormLOO        orpLOO
018800050722     c                   movel     ormPRO        orpPRO
018900050722     c                   movel     ormNAO        orpNAO
019000050722     c                   movel     ormCRA        orpCRA
019100050722     c                   movel     ormRSR        orpRSR
019200050722     c                   movel     ormINR        orpINR
019300050722     c                   movel     ormCAR        orpCAR
019400050722     c                   movel     ormLOR        orpLOR
019500050722     c                   movel     ormPRR        orpPRR
019600050722     c                   movel     ormNAR        orpNAR
019700050722     c                   movel     ormRER        orpRER
019800050722     c                   movel     ormTER        orpTER
019900050722     c                   movel     ormCRC        orpCRC
020000050722     c                   movel     ormRSC        orpRSC
020100050722     c                   movel     ormINC        orpINC
020200050722     c                   movel     ormCAC        orpCAC
020300050722     c                   movel     ormLOC        orpLOC
020400050722     c                   movel     ormPRC        orpPRC
020500050722     c                   movel     ormNAC        orpNAC
020600050722     c                   movel     ormFFD        orpFFD
020700050722     c                   movel     ormDAR        orpDAR
020800050722     c                   movel     ormORR        orpORR
020900050722     c                   movel     ormRMP        orpRMP
021000050722     c                   movel     ormNAM        orpNAM
021100050722     c                   movel     ormNCL        orpNCL
021200050722     c                   movel     ormPKG        orpPKG
021300050722     c                   movel     ormVLM        orpVLM
021400050722     c                   movel     ormBNC        orpBNC
021500050722     c                   movel     ormBLC        orpBLC
021600050722     c                   movel     ormATT        orpATT
021700050722     c                   movel     ormMTC        orpMTC
021800050722     c                   movel     ormPAG        orpPAG
021900050722     c                   movel     ormKSC        orpKSC
022000050722     c                   movel     ormCTR        orpCTR
022100050722     c                   movel     ormPOR        orpPOR
022200050722     c                   movel     ormZOR        orpZOR
022300050722     c                   movel     ormPOC        orpPOC
022400050722     c                   movel     ormNO1        orpNO1
022500050722     c                   movel     ormNO2        orpNO2
022600050722     c                   movel     ormDDT        orpDDT
022700050722     c                   movel     ormCST        orpCST
022800050722     c                   movel     ormVCS        orpVCS
022900050722     c                   movel     ormFCS        orpFCS
023000050722     c                   movel     ormDFO        orpDFO
023100050722     c                   movel     ormOFO        orpOFO
023200050722     c                   movel     ormSTO        orpSTO
023300050722     c                   movel     ormDST        orpDST
023400050722     c                   movel     ormNPG        orpNPG
023500050722     c                   movel     ormNDC        orpNDC
023600050722     c                   movel     ormDDC        orpDDC
023700050722     c                   movel     ormSTP        orpSTP
023800050722     c                   movel     ormRFA        orpRFA
023900050722     c                   movel     ormTAP        orpTAP
024000050722     c                   movel     ormETI        orpETI
024100050722     c                   movel     ormSPI        orpSPI
024200050722     c                   movel     ormFLO        orpFLO
024300060908      **?
024400061003     c                   z-add     999           orpFAO
024500050722     c                   eval      orpCAU = '80'
024600050722     c                   movel     G02DAT        orpDTV
024700050722     c                   movel     ore           orpORV
024800050722
024900050722     c                   write     fnORP000
025000060920
025100060920     c                   z-add     9             dpoSTS
025200060920     c                   z-add     0             dpoDTR
025300060920     c                   update    fiDPO000
025400060920
025500060907     c                   End
025600050715
025700050715      **?           ---------------
025800050715     c                   else
025900050715      **?           ---------------
026000050922      *
026100060908      **?  altrimenti non è ancora entrato nel giro degli ORM ed è ancora
026200060908      **?  da confermare:
026300050922     C/EXEC SQL
026400050922     C+ SELECT vaopoe, vaopoc INTO :LNPOE, :LNPOC FROM FNVAO00F WHERE
026500060811     C+ vaopag='O' and VAORFA = :RIFALFA15
026600050922     C/END-EXEC
026700060908      **?  se c'è :
026800060908     c                   if        lnpoe >0
026900050922      *
027000050922     c                   sorta     FIL
027100050922     c     lnPOC         lookup    FIL                                  3131
027200050922     c                   IF        not %equal
027300050922     c     lnPOE         lookup    FIL                                  3131
027400050922     c                   endIF
027500050922      *
027600050922     c                   IF        %Equal
027700061017      *
027800061017      **?  Prima di effettuare la cancellazione fisica del VAO chiamo un pgm che
027900061017      **?  scrive un record sugli ORM per tenere traccia del'ORM che è volutamente
028000061017      **?  è stato annullato su richiesta di DPD.
028100061017      **?   Se il programma ha dato esito positivo procedo con il DELETE fisico del VAO
028200061017      **?   altrimenti eseguo un annullamento logico del VAO impostando ad "A" il record
028300061017      **?   di annullamento.
028400061017      *
028500061017     c                   clear                   NumrecVAO         9 0
028600061017      **?  Reperisce Num.Rel.Record del VAO da passare alla routine
028700061017     C/EXEC SQL
028800061017     C+ SELECT rrn(FNVAO00F) INTO :NumrecVAO
028900061017     C+ FROM FNVAO00F WHERE
029000061017     C+ vaoPAG='O' and vaoRFA = :RIFALFA15
029100061017     C/END-EXEC
029200061018     c                   clear                   fieu48ds
029300061018     c                   z-add     NumrecVAO     ID48NRR
029400061018     c                   move      '0'           OD48ESITO
029500061017     c                   call      'FIEU48R'
029600061018     c                   PARM                    fieu48ds
029700061018     c                   move      OD48ESITO     TUTTokVAO         1
029800061018      *
029900050715      ** in questo caso invece non essendoci il legame si presume che l'ORM
030000050715      ** sia ancora sull'FNVAO e quindi occorre andare a cancellare il record.
030100050715      *
030200061017      **?  se Ok cancella fisicamente il VAO
030300061017     c                   if         TUTTokVAO = '0'
030400050715     C/EXEC SQL
030500061017     C+ delete  from FNVAO00F
030600061017     C+ where  vaopag='O' and VAORFA = :RIFALFA15
030700050715     C/END-EXEC
030800061017     c                   end
030900061017      **?  x Errore Annulla Logicamente il VAO
031000061017     c                   if         TUTTokVAO = '1'
031100061017     C/EXEC SQL
031200061017     C+ update FNVAO00F  set  VAOATB ='A'
031300061017     C+ where  vaopag='O' and VAORFA = :RIFALFA15
031400061017     C/END-EXEC
031500061017     c                   end
031600060914      *?      ATTENZIONE:  se la cancellazione è stata fatta OK
031700060908      *?          invio STATUS --> 3     impostando il flag a 13 x il FIEU41R
031800060908     c
031900060908      **?  prende un Nr.Parcel fittizio per inviare l'informazione corretta a DPD
032000060908     c                   clear                   trul33ds
032100060908     c                   z-add     25            i33cnu
032200060908     c                   z-add     1             i33num
032300060908     c                   movel     trul33ds      kpjbu
032400060908     c                   call      'TRUL33R'
032500060908     c                   PARM                    kpjba
032600060908     c                   movel     kpjbu         trul33ds
032700060908
032800060908     c     o33err        ifgt      *zeros
032900060908      **?  per errore non avanza lo stato del FIDPO
033000060908     c                   else
033100060908     c                   move      o33nri        dponrp
033200060908     c                   z-add     13            dpoSTS
033300060920     c                   z-add     0             dpoDTR
033400060908     c                   endif
033500060908      *
033600060908     c                   update    fiDPO000
033700060908      *
033800050922     c                   end
033900050715      *
034000060908     c                   end
034100060908      *
034200050715      **?           ---------------
034300050715     c                   end
034400050715      **?           ---------------
034500050704      *
034600061003     c                   end
034700061003      *
034800050705     c                   seton                                        lr
034900050704      **?------------------------------------------------------------------ */
