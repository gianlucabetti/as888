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
017400060810     C                   exsr      tivgd
017500050705
017600050704     c                   endsr
017700050920      **?__________________________________________________________________ */
017800050705     c     File_dpdD     begsr
017900050920      **?__________________________________________________________________ */
018000080711      *
018100080711      *   OCCORRE trascodificare la nostra Filiale di ritiro
018200080711      *     rimandando a DPD il loro codice nel RDEPOT e non 837 fisso
018300080711      *     come si era deciso di inviare al momento della partenza
018400080711      *     della procedura.
018500080711      *   QUINDI su organigramma vado a decodificare con che codice DPD
018600080711      *    viene vista la nostra filiale che esegue il ritiro.
018700080711      *
018800080718     c* introdotto il DPOFLR
018900080718     c*  contenente nei primi 4 bytes il Depot originariamente inviato da DPD
019000080718     c*   con il CRDATA01 --> che loro vogliono ASSOLUTAMENTE.
019100080718      *  altrimenti procedo cercando di inviargli il Depot Ritirante REALE
019200080718     c                   if        %subst(dpoFLR:1:4) <> *blank
019300080718      *
019400080718     c                   eval      RDEPOT = %subst(dpoFLR:1:4)
019500080718      *
019600080718     c                   else
019700080718      *
019800080711      * prepara la chiave per agganciare l'orm
019900080711     c                   movel(p)  '0837'        Rdepot            4
020000080711     c                   clear                   alfa10
020100080711     c                   movel     dpodep        alfa10
020200080711     c                   move      dpoord        alfa10
020300080711     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
020400080711      * aggancia l'orm Bartolini
020500080711     c     alfa15        chain     fnorm07l
020600080711      **?           ---------------
020700080711     c                   if        %Found(fnORM07l)
020800080711      *
020900080711     c     OrmPor        chain     azOrg01l
021000080711     c                   if        %found(azOrg01l)
021100080711     c                   movel     orgde3        og143
021200080711     c                   movel(p)  §OGDP1        Rdepot
021300080711     c                   endIf
021400080711      *
021500080711     c                   end
021600080711      **?           ---------------
021700080718     c                   endIf
021800080711      *
021900060920      * se stato 14 si deve impostare il mancato ritiro sul record da inviare
022000060810      *   anche come codice aggiuntivo negli SCAN .
022100080515      ***
022200080515      *  il 15/5/2008 René Herold ha spiegato che nel CRDATA02 si deve inviare
022300080515      *  solo lo Status 1 con il Parcel per indicare qualsiasi situazione
022400080515      *  le specifiche asteriscate il 15/5/2008 servono quindi per modificare
022500080515      *  il tipo di invio da effettuare a fronte di questo nuovo chiarimento.
022600080515      ***
022700060920     c                   if        status = 11
022800060920     c                   move      '1'           stato_dpd
022900060920     c                   end
023000080515      ***
023100060920     c                   if        status = 12
023200080515      ***  Se ritirato Ok si deve inviare lo status 1 con il legame del Nr.Parcel
023300080515     c*******            move      '2'           stato_dpd
023400080515     c                   move      '1'           stato_dpd
023500060908     c                   end
023600080515      ***
023700080515      ***  Se si tratta di una cancellazione ORM OK non si deve inviare nulla.
023800060920     c                   if        status = 13
023900060920     c                   move      '3'           stato_dpd
024000060920     c                   end
024100080515      ***
024200080515      ***  Se si tratta di una Orm non riuscito occorre mandare lo status 1
024300080515      ***    con il legame del Nr.Parcel
024400080515      ***    Poi verrà mandato lo SCAN 18 con il motivo legato a quel Parcel.
024500060920     c                   if        status = 14
024600060920     c                   exsr      Man_Ritiro
024700080515     c*******            move      '4'           stato_dpd
024800080515     c                   move      '1'           stato_dpd
024900060920     c                   end
025000080515      ***
025100080515      ***  Se si tratta di una cancellazione ORM non OK si deve inviare
025200080515      ***   lo status 1 con il legame del Nr.Parcel
025300060920     c                   if        status = 15
025400080515     c*******            move      '5'           stato_dpd
025500080515     c                   move      '1'           stato_dpd
025600060920     c                   end
025700050705      * dati orm
025800061006     c                   move      dpoord        dpoordA           6
025900050705     c                   eval      DPorecord =
026000060810     c                             %trim(dpodep) + ';' +
026100080522     c***********                  %trim(dpoordA) + ';' +
026200080711     c                             %trim(dpoordA) + ';' +
026300080718     c                             %trim(Rdepot) + ';' +
026400080522     c***********                  %trim(dpodep) + ';' +
026500060810     c                             %trim(dponrp) + ';;;' +
026600060810     c                             %trim(stato_dpd) + ';;;;'
026700080515      **
026800060810      *  coda con Mancato Ritiro  oppure tutto OK
026900080515     c*********          if        status = 14
027000080515     c*********          eval      DPorecord = %trim(DPorecord) + '18;' +
027100080515     c*********                    %trim(mancod) + ';'
027200080515     c*********          else
027300060810     c                   eval      DPorecord = %trim(DPorecord) + ';;'
027400080515     c*********          end
027500060810
027600080515      *** A questo punto inviamo solo dei records di STATO 1 con il legame
027700080515      **   del Nr.Parcel e niente altro. Così vuole DPD. (15/5/2008)
027800080515     c                   if        stato_dpd = '1'
027900060810     C                   exsr      tivgd
028000080515     c                   end
028100050705
028200050705     c                   endsr
028300050920      **?__________________________________________________________________ */
028400050705     c     File_dpdP     begsr
028500050920      **?__________________________________________________________________ */
028600050705      * piede messaggio
028700050705     c                   eval      DPorecord =
028800050705     c                             '# END;' +
028900050705     c                             %trim(%editc(progr:'Z')) + ';'
029000060810     C                   exsr      tivgd
029100050705
029200050705     c                   endsr
029300050920      **?__________________________________________________________________ */
029400050705     c     numero        begsr
029500050920      **?__________________________________________________________________ */
029600050705     c                   eval      kpjbus=kpjbu
029700050705     c                   clear                   trul33ds
029800050705     c                   move      'L'           i33tla
029900050705     c                   z-add     305           i33cnu
030000050705     c                   z-add     1             i33num
030100050705     c                   movel     trul33ds      kpjbu
030200050705     c                   call      'TRUL33R'
030300050705     c                   PARM                    kpjba
030400050705     c                   movel     kpjbu         trul33ds
030500050705     c                   z-add     o33nri        progr
030600050705     c                   eval      kpjbu=kpjbus
030700050705
030800050705     c                   endsr
030900050920      **?__________________________________________________________________ */
031000060920     c     Test_Annull   begsr
031100050920      **?__________________________________________________________________ */
031200060920      *  Controlla se la proposta di annullamento ha generato la chiusura
031300060920      *   dell'ORM
031400060920      *
031500061005      * prepara la chiave per agganciare l'orm
031600061005     c                   clear                   alfa10
031700061005     c                   movel     dpodep        alfa10
031800061005     c                   move      dpoord        alfa10
031900061005     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
032000061005
032100061005      * aggancia l'orm Bartolini
032200061005     c     alfa15        chain     fnorm07l
032300061005      **?           ---------------
032400061005     c                   if        %Found(fnORM07l)
032500061005      **?           ---------------
032600060920      *  chiuso (Stornato)
032700060920     c                   select
032800060920     c                   when      ormFAO = 999
032900060920     c                   z-add     13            dpoSTS
033000060920     c                   z-add     0             dpoDTR
033100060920      *
033200060920      *  avanzato di fase (non è possibile + eseguire uno Storno)
033300060920     c                   when      ormFAO >= 600
033400060920     c                   z-add     15            dpoSTS
033500060920     c                   z-add     0             dpoDTR
033600060920     c                   endsl
033700060920      *
033800060920     c                   endif
033900060920
034000060920     c                   endsr
034100060920      **?__________________________________________________________________ */
034200060920     c     Man_Ritiro    begsr
034300060920      **?__________________________________________________________________ */
034400060920     c*
034500060810     c                   clear                   mancod
034600060810      * ricerca su orm/orf motivo mancata consegna con ORM e fase 999
034700060810     c     korm          chain     fnorm01l
034800060810     c                   if        %found(fnorm01l) and ormfao = fas999
034900060810     c     korf          chain     fnorf03l
035000060810     c                   if        %found(fnorf03l)
035100060810     c                   z-add     1             x
035200060810     c     orfcar        lookup    manb(x)                                66
035300060810     c                   movel     mand(x)       mancod
035400060810     c                   endif
035500060810     c                   endif
035600060810
035700060810     c                   endsr
035800060810      **?__________________________________________________________________ */
035900060810     c     *inzsr        begsr
036000060810      **?__________________________________________________________________ */
036100060810     c*
036200050705     c     *entry        plist
036300050705     C                   parm                    kpjba
036400050705     C                   TIME                    ORADAT           14 0
036500050705     C                   MOVEL     ORADAT        ORATR             6 0
036600060810     c                   move      *date         udtymd            8 0
036700060810     C                   TIME                    W0140
036800060810     C                   MOVE      W0140         W0080             8 0
036900060810
037000060810     c     korm          klist
037100060810     c                   kfld                    DPOPOE
037200060810     c                   kfld                    DPONRS
037300060810     c                   kfld                    DPONOR
037400060810     c                   kfld                    DPONRV
037500060920
037600060810     c     korf          klist
037700060810     c                   kfld                    ormPOE
037800060810     c                   kfld                    ormNsr
037900060810     c                   kfld                    ormNOR
038000060810     c                   kfld                    ormNRV
038100060810     c                   kfld                    fas999
038200060810
038300060810      **?carica motivi mancata consegna                                     */
038400060810     c     'MRD'         setll     tntbe01l
038500060810     c                   do        *hival
038600060810     c     'MRD'         reade     tntbe01l
038700060810     c                   if        %eof(tntbe01l)
038800060810     c                   leave
038900060810     c                   endif
039000060810     c                   movel     tbeuni        dmrd
039100060810     c                   add       1             x
039200060810     c                   movel     tbeke1        manb(x)
039300060810     c                   movel     §mrdcar       mand(x)
039400060810     c                   enddo
039500050704
039600050512     c                   endsr
039700060810      *----------------------------------------------------*
039800060810      *   Scrittura tivgd file da trsmettere               *
039900060810      *----------------------------------------------------*
040000060810     C     tivgd         Begsr
040100060810      *
040200060810     c                   clear                   tivgd000
040300080403     c                   eval      vgddta = %TrimR(dpoRecord)
040400060810     c                   eval      vgdtip = '$7'
040500060810     c                   eval      vgdksu = '0DPD0OUT'
040600060810     c                   eval      vgdtsc = 'WW'
040700060810     c                   eval      vgdpgm = 'FIEU41R'
040800061023     c                   eval      vgddat = udtymd
040900060810      *
041000060810     C                   WRITE     tivgd000
041100060810      *
041200060810     C                   Endsr
041300060810      *----------------------------------------------------*
