000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200050705     H* Fieu41R *----------------------------------------------------*
000300900418     H*                                                              *
000400060810     H* Scrittura del CRDATA02 tramite DOWNLOAD                      *
000500060810     h* record in stato 10/20 restituzione a dpd numero parcel       *
000600060810     h* abbinato all'ORM                                             *
000700060810     H* -------------------------------------------------------------*
000800050705     Ffidpo02l  uf   E           k DISK    commit
000900060810     Ffnorm01l  if   E           k DISK
001000061005     Ffnorm07l  if   E           k DISK    rename(fnorm000:fnorm7)
001100060810     Ffnorf03l  if   E           k DISK
001200060810     Ftntbe01l  if   E           k DISK
001300080711     FazOrg01l  if   E           k DISK
001400060810
001500080424     ftivgd00f  if a E             DISK    commit
001600080403     D*** dpoRecord       S                   LIKE(vgddta)
001700080403     D dpoRecord       S           2048
001800060810     d trul47ds      e ds
001900060810     D W0140           S             14  0 inz
002000060810
002100900418     D KPJBA         E DS
002200050705     D trul33ds      E DS
002300060810     D dmrd          E DS
002400080711     D OG143         E DS
002500050704
002600050705     D KPJBus          s                   like(kpjbu)
002700050705     D progr           s              7s 0
002800050705     D primo           s              1
002900060810     D fas999          s                   like(orffar) inz(999)
003000060920     D status          s                   like(dposts)
003100060810     D x               s              3s 0
003200060810     D mancod          s              3
003300060810     D manb            s              2    dim(50)
003400060810     D mand            s              3s 0 dim(50)
003500060810     D stato_dpd       s              1
003600061005
003700061005     D alfa10          s             10
003800061005     D alfa15          s             15
003900050920      **?__________________________________________________________________ */
004000050704      *    main
004100050920      **?__________________________________________________________________ */
004200060920      **?Pre-ciclo di controllo e modifica  STATUS da inviare in seguito
004300060920      **?con il ciclo successivo.
004400060920     c                   z-add     9             status
004500050705     c     status        setll     fidpo02l
004600050705     c                   do        *hival
004700050705     c     status        reade     fidpo02l
004800050705     c                   if        %eof(fidpo02l)
004900050705     c                   leave
005000050705     c                   endif
005100060920
005200060920      * se status 9 di proposto annullamento deve verificare se l'ORM è stato
005300060920      * annullato oppure se non può essere annullato  quindi cambiare lo status
005400060920      * da 9 a 13 oppure 15.
005500060920     c                   if        status = 9
005600060920     c                   exsr      Test_Annull
005700060920     c                   endif
005800060920
005900060920     c                   update    fidpo000                             99
006000060920     c                   enddo
006100060920      **?__________________________________________________________________ */
006200060920      **?Ciclo di scrittura del file x Download legami/Stati
006300060920     c                   z-add     11            status
006400060920      * <<<<<
006500060920     c     riparti       tag
006600060920
006700060920     c     status        setll     fidpo02l
006800060920     c                   do        *hival
006900060920     c     status        reade     fidpo02l
007000060920     c                   if        %eof(fidpo02l)
007100060920     c                   leave
007200060920     c                   endif
007300060920
007400050705      * operazioni da eseguire una sola volta reperimento numero di
007500050705      * trasmissione e intestazione del file da inviare
007600050705     c                   if        primo = *blank
007700050705     c                   exsr      file_dpdt
007800050705     c                   move      'X'           primo
007900050705     c                   endif
008000050705
008100050705     c                   exsr      file_dpdD
008200060810
008300060920      *
008400060920      ** non viene flaggato il record x il mancato ritiro (14) poichè serve al
008500060920      **  FIEU42R chiamato prima del SETON LR x scrivere il 2°file di DOWNLOAD
008600060920      **  che imposterà la data trasmissione.
008700060920     c                   if        status = 14
008800060920     c                   move      mancod        DpoCdMR
008900060920     c                   else
009000060920      **?  con DTR > 0 il record viene dato x trasmesso x tutti gli altri stati <> da 14
009100060920     c                   move      *date         DPODTR
009200060920     c                   end
009300060920      **
009400050705     c                   z-add     progr         DPONTR
009500060810
009600050704     c                   update    fidpo000                             99
009700050705     c                   enddo
009800060920      * <<<<<
009900060920     c                   if        status = 11
010000060920     c                   z-add     12            status
010100060920     c                   goto      riparti
010200060920     c                   endif
010300060908      * <<<<<
010400060920     c                   if        status = 12
010500060920     c                   z-add     13            status
010600060908     c                   goto      riparti
010700060908     c                   endif
010800060914      * <<<<<
010900060920     c                   if        status = 13
011000060920     c                   z-add     14            status
011100060914     c                   goto      riparti
011200060914     c                   endif
011300060810
011400060920     c                   if        status = 14
011500060920     c                   z-add     15            status
011600060920     c                   goto      riparti
011700060920     c                   endif
011800060920
011900050919      * chiusura file da inviare solo se ho trovato almeno un record valido
012000050707     c                   if        primo <> *blank
012100060810     C*
012200050707     c                   exsr      file_dpdP
012300060920     c                   commit
012400060810     C*
012500060810     C* Infine elimino il blocco elaborazione TIVGD
012600060810     C                   clear                   trul47ds
012700060810     C                   eval      d47opz  = 'F'
012800060810     C                   eval      d47tip  = '$7'
012900060810     C                   call      'TRUL47R'
013000060810     C                   parm                    trul47ds
013100060810     C*
013200050707     c                   endif
013300050704      *
013400060906      *  chiama il programma per scrivere gli SCAN 18 dei mancati ritiri
013500060906     c                   call      'FIEU42R'
013600060906     c                   parm                    kpjba
013700060920      *
013800060906      *
013900050705     c                   seton                                        lr
014000050920      **?__________________________________________________________________ */
014100050705     c     File_dpdT     begsr
014200050920      **?__________________________________________________________________ */
014300060810
014400060810      *  istruzioni apertura blocco scrittura TIVGD
014500060810     C                   clear                   trul47ds
014600060810     C                   eval      d47opz  = 'I'
014700060810     C                   eval      d47tip  = '$7'
014800060810     C                   eval      d47lck  = 'N'
014900060810     C                   eval      d47chkj = 'N'
015000060810     C                   eval      d47pgm  = 'FIEU41R'
015100060810     C                   call      'TRUL47R'
015200060810     C                   parm                    trul47ds
015300060810
015400060810      * preleva il numero Parcel
015500050705     c                   exsr      numero
015600050705      * testata depot
015700050705     c                   eval      DPorecord =
015800060920     c                             '# FILE;DEPOT0837;0837;' +
015900050705     c                             %trim(%editc(*date:'Z')) + ';' +
016000050705     c                             %trim(%editc(oratr:'Z')) + ';' +
016100050705     c                             %trim(%editc(progr:'Z'))+ ';'
016200060810     C                   exsr      tivgd
016300050705
016400060810      * testata depot
016500060810     c                   eval      DPorecord =
016600060814     c                             '# VERSION;0;'
016700060810     C                   exsr      tivgd
016800060810
016900050705      * testata depot
017000050705     c                   eval      DPorecord =
017100060810     c                             '# DEF;CR:CRDATA02;ODEPOT;ORDERNR;RDEPOT;-
017200060810     c                             PKNR;NEUDAT;PICKUPDATE;STATUS;WEIGHT;SERVICE-
017300060810     c                             ;CUSTOM;SCANTYPE;ZCODE;'
017400090617      *
017500090617     c                   if        udtymd > 20090809
017600090617     c                   eval      DPorecord = %Trim(DPorecord) +
017700090617     c                             'CUSTREF;CONFIRM;CANCELUSER;;'
017800090617     c                   end
017900090617      *
018000060810     C                   exsr      tivgd
018100050705
018200050704     c                   endsr
018300050920      **?__________________________________________________________________ */
018400050705     c     File_dpdD     begsr
018500050920      **?__________________________________________________________________ */
018600080711      *
018700080711      *   OCCORRE trascodificare la nostra Filiale di ritiro
018800080711      *     rimandando a DPD il loro codice nel RDEPOT e non 837 fisso
018900080711      *     come si era deciso di inviare al momento della partenza
019000080711      *     della procedura.
019100080711      *   QUINDI su organigramma vado a decodificare con che codice DPD
019200080711      *    viene vista la nostra filiale che esegue il ritiro.
019300080711      *
019400080718     c* introdotto il DPOFLR
019500080718     c*  contenente nei primi 4 bytes il Depot originariamente inviato da DPD
019600080718     c*   con il CRDATA01 --> che loro vogliono ASSOLUTAMENTE.
019700080718      *  altrimenti procedo cercando di inviargli il Depot Ritirante REALE
019800080718     c                   if        %subst(dpoFLR:1:4) <> *blank
019900080718      *
020000080718     c                   eval      RDEPOT = %subst(dpoFLR:1:4)
020100080718      *
020200080718     c                   else
020300080718      *
020400080711      * prepara la chiave per agganciare l'orm
020500080711     c                   movel(p)  '0837'        Rdepot            4
020600080711     c                   clear                   alfa10
020700080711     c                   movel     dpodep        alfa10
020800080711     c                   move      dpoord        alfa10
020900080711     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
021000080711      * aggancia l'orm Bartolini
021100080711     c     alfa15        chain     fnorm07l
021200080711      **?           ---------------
021300080711     c                   if        %Found(fnORM07l)
021400080711      *
021500080711     c     OrmPor        chain     azOrg01l
021600080711     c                   if        %found(azOrg01l)
021700080711     c                   movel     orgde3        og143
021800080711     c                   movel(p)  §OGDP1        Rdepot
021900080711     c                   endIf
022000080711      *
022100080711     c                   end
022200080711      **?           ---------------
022300080718     c                   endIf
022400080711      *
022500060920      * se stato 14 si deve impostare il mancato ritiro sul record da inviare
022600060810      *   anche come codice aggiuntivo negli SCAN .
022700080515      ***
022800080515      *  il 15/5/2008 René Herold ha spiegato che nel CRDATA02 si deve inviare
022900080515      *  solo lo Status 1 con il Parcel per indicare qualsiasi situazione
023000080515      *  le specifiche asteriscate il 15/5/2008 servono quindi per modificare
023100080515      *  il tipo di invio da effettuare a fronte di questo nuovo chiarimento.
023200080515      ***
023300060920     c                   if        status = 11
023400060920     c                   move      '1'           stato_dpd
023500060920     c                   end
023600080515      ***
023700060920     c                   if        status = 12
023800080515      ***  Se ritirato Ok si deve inviare lo status 1 con il legame del Nr.Parcel
023900080515     c*******            move      '2'           stato_dpd
024000080515     c                   move      '1'           stato_dpd
024100060908     c                   end
024200080515      ***
024300080515      ***  Se si tratta di una cancellazione ORM OK non si deve inviare nulla.
024400060920     c                   if        status = 13
024500060920     c                   move      '3'           stato_dpd
024600060920     c                   end
024700080515      ***
024800080515      ***  Se si tratta di una Orm non riuscito occorre mandare lo status 1
024900080515      ***    con il legame del Nr.Parcel
025000080515      ***    Poi verrà mandato lo SCAN 18 con il motivo legato a quel Parcel.
025100060920     c                   if        status = 14
025200060920     c                   exsr      Man_Ritiro
025300080515     c*******            move      '4'           stato_dpd
025400080515     c                   move      '1'           stato_dpd
025500060920     c                   end
025600080515      ***
025700080515      ***  Se si tratta di una cancellazione ORM non OK si deve inviare
025800080515      ***   lo status 1 con il legame del Nr.Parcel
025900060920     c                   if        status = 15
026000080515     c*******            move      '5'           stato_dpd
026100080515     c                   move      '1'           stato_dpd
026200060920     c                   end
026300050705      * dati orm
026400061006     c                   move      dpoord        dpoordA           6
026500050705     c                   eval      DPorecord =
026600060810     c                             %trim(dpodep) + ';' +
026700080522     c***********                  %trim(dpoordA) + ';' +
026800080711     c                             %trim(dpoordA) + ';' +
026900080718     c                             %trim(Rdepot) + ';' +
027000080522     c***********                  %trim(dpodep) + ';' +
027100060810     c                             %trim(dponrp) + ';;;' +
027200060810     c                             %trim(stato_dpd) + ';;;;'
027300080515      **
027400060810      *  coda con Mancato Ritiro  oppure tutto OK
027500080515     c*********          if        status = 14
027600080515     c*********          eval      DPorecord = %trim(DPorecord) + '18;' +
027700080515     c*********                    %trim(mancod) + ';'
027800080515     c*********          else
027900060810     c                   eval      DPorecord = %trim(DPorecord) + ';;'
028000080515     c*********          end
028100060810
028200080515      *** A questo punto inviamo solo dei records di STATO 1 con il legame
028300080515      **   del Nr.Parcel e niente altro. Così vuole DPD. (15/5/2008)
028400080515     c                   if        stato_dpd = '1'
028500090617      *
028600090617     c                   if        udtymd > 20090809
028700090617     c                   eval      DPorecord = %Trim(DPorecord) + ';;;;'
028800090617     c                   end
028900090617      *
029000060810     C                   exsr      tivgd
029100080515     c                   end
029200050705
029300050705     c                   endsr
029400050920      **?__________________________________________________________________ */
029500050705     c     File_dpdP     begsr
029600050920      **?__________________________________________________________________ */
029700050705      * piede messaggio
029800050705     c                   eval      DPorecord =
029900050705     c                             '# END;' +
030000050705     c                             %trim(%editc(progr:'Z')) + ';'
030100060810     C                   exsr      tivgd
030200050705
030300050705     c                   endsr
030400050920      **?__________________________________________________________________ */
030500050705     c     numero        begsr
030600050920      **?__________________________________________________________________ */
030700050705     c                   eval      kpjbus=kpjbu
030800050705     c                   clear                   trul33ds
030900050705     c                   move      'L'           i33tla
031000050705     c                   z-add     305           i33cnu
031100050705     c                   z-add     1             i33num
031200050705     c                   movel     trul33ds      kpjbu
031300050705     c                   call      'TRUL33R'
031400050705     c                   PARM                    kpjba
031500050705     c                   movel     kpjbu         trul33ds
031600050705     c                   z-add     o33nri        progr
031700050705     c                   eval      kpjbu=kpjbus
031800050705
031900050705     c                   endsr
032000050920      **?__________________________________________________________________ */
032100060920     c     Test_Annull   begsr
032200050920      **?__________________________________________________________________ */
032300060920      *  Controlla se la proposta di annullamento ha generato la chiusura
032400060920      *   dell'ORM
032500060920      *
032600061005      * prepara la chiave per agganciare l'orm
032700061005     c                   clear                   alfa10
032800061005     c                   movel     dpodep        alfa10
032900061005     c                   move      dpoord        alfa10
033000061005     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
033100061005
033200061005      * aggancia l'orm Bartolini
033300061005     c     alfa15        chain     fnorm07l
033400061005      **?           ---------------
033500061005     c                   if        %Found(fnORM07l)
033600061005      **?           ---------------
033700060920      *  chiuso (Stornato)
033800060920     c                   select
033900060920     c                   when      ormFAO = 999
034000060920     c                   z-add     13            dpoSTS
034100060920     c                   z-add     0             dpoDTR
034200060920      *
034300060920      *  avanzato di fase (non è possibile + eseguire uno Storno)
034400060920     c                   when      ormFAO >= 600
034500060920     c                   z-add     15            dpoSTS
034600060920     c                   z-add     0             dpoDTR
034700060920     c                   endsl
034800060920      *
034900060920     c                   endif
035000060920
035100060920     c                   endsr
035200060920      **?__________________________________________________________________ */
035300060920     c     Man_Ritiro    begsr
035400060920      **?__________________________________________________________________ */
035500060920     c*
035600060810     c                   clear                   mancod
035700060810      * ricerca su orm/orf motivo mancata consegna con ORM e fase 999
035800060810     c     korm          chain     fnorm01l
035900060810     c                   if        %found(fnorm01l) and ormfao = fas999
036000060810     c     korf          chain     fnorf03l
036100060810     c                   if        %found(fnorf03l)
036200060810     c                   z-add     1             x
036300060810     c     orfcar        lookup    manb(x)                                66
036400060810     c                   movel     mand(x)       mancod
036500060810     c                   endif
036600060810     c                   endif
036700060810
036800060810     c                   endsr
036900060810      **?__________________________________________________________________ */
037000060810     c     *inzsr        begsr
037100060810      **?__________________________________________________________________ */
037200060810     c*
037300050705     c     *entry        plist
037400050705     C                   parm                    kpjba
037500050705     C                   TIME                    ORADAT           14 0
037600050705     C                   MOVEL     ORADAT        ORATR             6 0
037700060810     c                   move      *date         udtymd            8 0
037800060810     C                   TIME                    W0140
037900060810     C                   MOVE      W0140         W0080             8 0
038000060810
038100060810     c     korm          klist
038200060810     c                   kfld                    DPOPOE
038300060810     c                   kfld                    DPONRS
038400060810     c                   kfld                    DPONOR
038500060810     c                   kfld                    DPONRV
038600060920
038700060810     c     korf          klist
038800060810     c                   kfld                    ormPOE
038900060810     c                   kfld                    ormNsr
039000060810     c                   kfld                    ormNOR
039100060810     c                   kfld                    ormNRV
039200060810     c                   kfld                    fas999
039300060810
039400060810      **?carica motivi mancata consegna                                     */
039500060810     c     'MRD'         setll     tntbe01l
039600060810     c                   do        *hival
039700060810     c     'MRD'         reade     tntbe01l
039800060810     c                   if        %eof(tntbe01l)
039900060810     c                   leave
040000060810     c                   endif
040100060810     c                   movel     tbeuni        dmrd
040200060810     c                   add       1             x
040300060810     c                   movel     tbeke1        manb(x)
040400060810     c                   movel     §mrdcar       mand(x)
040500060810     c                   enddo
040600050704
040700050512     c                   endsr
040800060810      *----------------------------------------------------*
040900060810      *   Scrittura tivgd file da trsmettere               *
041000060810      *----------------------------------------------------*
041100060810     C     tivgd         Begsr
041200060810      *
041300060810     c                   clear                   tivgd000
041400080403     c                   eval      vgddta = %TrimR(dpoRecord)
041500060810     c                   eval      vgdtip = '$7'
041600060810     c                   eval      vgdksu = '0DPD0OUT'
041700060810     c                   eval      vgdtsc = 'WW'
041800060810     c                   eval      vgdpgm = 'FIEU41R'
041900061023     c                   eval      vgddat = udtymd
042000060810      *
042100060810     C                   WRITE     tivgd000
042200060810      *
042300060810     C                   Endsr
042400060810      *----------------------------------------------------*
