000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200051007     H* Fieu42R *----------------------------------------------------*
000300900418     H*                                                              *
000400080515     H* Scrittura SCANDATA  da aggiungere agli SCAN di delivery      *
000500050919     h* record in stato 10 DA PORTARE A 18 X MANCATI RITIRI          *
000600060811      **?___________________________________________________________ *
000700050919     Ffidpo04l  uf   E           k DISK    commit
000800080424     ftivgd00f  if a E             DISK    commit
000900080529     Ffnorm01l  if   E           k DISK
001000080529     Fazorg01l  if   E           k DISK
001100080529      **?___________________________________________________________ *
001200060810
001300080403     D** dpoRecord       S                   LIKE(vgddta)
001400080403     D dpoRecord       S           2048
001500060810     d trul47ds      e ds
001600060810     D W0140           S             14  0 inz
001700060810      *
001800900418     D KPJBA         E DS
001900080529     D OG143         E DS
002000050705     D trul33ds      E DS
002100060810     D Lung_Parcel     S              3  0
002200060811     D sav_lung_par    S              3  0 inz(0)
002300060811     D Almeno_uno      S              1
002400050704
002500050705     D KPJBus          s                   like(kpjbu)
002600060811     D solo_inizio     s              1    inz(' ')
002700060920     D status          s                   like(dposts)
002800060811     D savDEP          S                   like(dpoDEP)
002900050920      **?__________________________________________________________________ */
003000050704      *    main
003100050920      **?__________________________________________________________________ */
003200060811      **
003300060920     c                   z-add     14            status
003400050927     c     status        setll     fidpo04l
003500050927     c     status        reade     fidpo04l
003600060811      **
003700060811     c                   dow       not %EoF(fidpo04l)
003800060920      * se già trasmesse  bypassa
003900060920     c                   if        dpodtr = 0
004000060810      ** controlla lunghezza del Parcel
004100060810     c                   eval      Lung_Parcel = %Len(%trim(dpoNRP))
004200060810
004300050705      * operazioni da eseguire una sola volta reperimento numero di
004400050705      * trasmissione e intestazione del file da inviare
004500060811     c                   if        Lung_Parcel <> sav_lung_par  or
004600060811     c                             dpoDEP      <> savDEP
004700060811     c                   exsr      Testata
004800060811     c                   eval      sav_lung_par  = Lung_Parcel
004900060811     c                   eval      SavDEP        = dpoDEP
005000050705     c                   endif
005100080529      *
005200080529      *  Deve reperire il Collecting DEPOT che ha dato il Mancato Ritiro
005300080529      *   da impostare nello SCAN e lo decodifica dall'organigramma:
005400080529      *    mette di Default Verona
005500080529     c                   movel     '0837'        Coll_Depot        4
005600080529     c     korm          chain     fnorm01l
005700080529     c                   if        %found(fnorm01l)
005800080529      *  con la Filiale Ritirante
005900080529     c     ormPOR        chain     azorg01l
006000080529     c                   if        %found(azorg01l)
006100080529     c                   movel     orgDE3        Og143
006200080529     c                   if        §OGDP1 <> *blank
006300080529     c                   movel     §OGDP1        Coll_Depot
006400080529     c                   endif
006500080529     c                   endif
006600080529     c                   endif
006700060810      * dettaglio
006800060811     c                   move      'S'           almeno_uno
006900060811     c                   exsr      Dettaglio
007000060810
007100050705     c                   move      *date         DPODTR
007200050919     c                   update    fidpo000                             99
007300060811     c                   endif
007400060811
007500060811     c     status        reade     fidpo04l
007600050705     c                   enddo
007700060810
007800050919      * chiusura file da inviare solo se ho trovato almeno un record valido
007900060811     c                   exsr      Coda
008000050707     c                   commit
008100060810     C*
008200060810     C* Infine elimino il blocco elaborazione TIVGD
008300060811     c                   if        almeno_uno = 'S' and solo_inizio = 'S'
008400060810     C                   clear                   trul47ds
008500060810     C                   eval      d47opz  = 'F'
008600060810     C                   eval      d47tip  = '$3'
008700060810     C                   call      'TRUL47R'
008800060810     C                   parm                    trul47ds
008900060811     c                   endif
009000050704      *
009100050705     c                   seton                                        lr
009200050920      **?__________________________________________________________________ */
009300060811     c     Testata       begsr
009400050920      **?__________________________________________________________________ */
009500060811      *  istruzioni apertura blocco scrittura TIVGD
009600060811      *    fatto solo all'inizio del programma
009700060811     c                   if        solo_inizio = *blank
009800060811     C                   clear                   trul47ds
009900060811     C                   eval      d47opz  = 'I'
010000060811     C                   eval      d47tip  = '$3'
010100060811     C                   eval      d47lck  = 'N'
010200060811     C                   eval      d47chkj = 'N'
010300060811     C                   eval      d47pgm  = 'FIEU42R'
010400060811     C                   call      'TRUL47R'
010500060811     C                   parm                    trul47ds
010600060811     c                   eval      solo_inizio = 'S'
010700060811     c                   end
010800060811
010900060811      * a cambio di tipo Parcel --> cambio di SCAN
011000060811      *   chiude con il record di Fine
011100060811     c                   if        lung_parcel <> sav_lung_par  or
011200060811     c                             dpoDEP      <> savDEP
011300060811     c                   exsr      Coda
011400060811     c                   end
011500060811
011600050920      *rekord testata scan
011700060810     c                   clear                   dpoRecord
011800060811
011900060811     c                   if        lung_parcel = 11
012000060811      *
012100051007     c                   eval      dporecord ='0818837001' +
012200061006     c                             udtymda + oratra + '000000' + '|'
012300060811     c                   else
012400060811     c                   eval      dporecord ='0818' +
012500061006     c                             udtymda + oratra +
012600061027     c                             '|D' + %trim(dpoDEP) + '|T001|'
012700060811     c                   end
012800050920      *
012900060810     C                   exsr      tivgd
013000050920      *
013100050920     c                   endsr
013200050920      **?__________________________________________________________________ */
013300060811     c     Dettaglio     begsr
013400050920      **?__rekord dettaglio scan __________________________________________ */
013500050920
013600050920      * prepara scan per invio
013700060811     c                   clear                   dporecord
013800060811
013900060811     c                   if        lung_parcel = 11
014000060811      *
014100051007     c                   eval      dporecord ='0118837001' +
014200061006     c                             udtymda + oratra + '000000' +
014300060811     c                             %trim(dpoNRP)+ '|Z' + %trim(dpoCDmr)+ '|'
014400060811     c                   else
014500061003     c                   eval      dporecord ='0118' +
014600061006     c                             udtymda + oratra +
014700080529     c                             '|D' + %trim(Coll_Depot) + '|T001' +
014800060811     c                             '|P' + %trim(dpoNRP) +
014900060915     c                             '|Z' + %trim(dpoCDmr)+
015000080529     c                             '|YAL01|YAD03' + %subst(dpoCDmr:2:2) + '|'
015100080527     c
015200060811     c                   end
015300060811      *
015400060810     C                   exsr      tivgd
015500060810
015600050920     c                   endsr
015700050920      **?__________________________________________________________________ */
015800060811     c     Coda          begsr
015900050920      **?-piede messaggio          ________________________________________ */
016000060811     c                   if        almeno_uno = 'S'
016100060811      *
016200060811     c                   clear                   dpoRecord
016300060811      *
016400060811     c                   if        sav_lung_par  = 11
016500060811      *
016600051007     c                   eval      dporecord ='0918837001' +
016700061006     c                             udtymda + oratra +'000000' + '|'
016800060811     c                   else
016900060811      *
017000060811     c                   eval      dporecord ='0918' +
017100061006     c                             udtymda + oratra +
017200061027     c                             '|D' + %trim(savDEP) + '|T001|'
017300060811     c                   end
017400050920      *
017500060810     C                   exsr      tivgd
017600060811
017700060811     c                   end
017800060810
017900050920     c                   endsr
018000050920      **?__________________________________________________________________ */
018100050512     c     *inzsr        begsr
018200050920      **?__________________________________________________________________ */
018300050705     c     *entry        plist
018400050705     C                   parm                    kpjba
018500060920      ***
018600080529     c     korm          klist
018700080529     c                   kfld                    DPOPOE
018800080529     c                   kfld                    DPONRS
018900080529     c                   kfld                    DPONOR
019000080529     c                   kfld                    DPONRV
019100080529
019200050705     C                   TIME                    ORADAT           14 0
019300050705     C                   MOVEL     ORADAT        ORATR             6 0
019400061006     C                   MOVEL     ORATR         ORATRa            6
019500050920     c                   move      *date         udtymd            8 0
019600061006     c                   move      udtymd        udtymda           8
019700060810     C                   TIME                    W0140
019800060810     C                   MOVE      W0140         W0080             8 0
019900050920
020000050512     c                   endsr
020100060810      **?__________________________________________________________________ */
020200060810      *   Scrittura tivgd file da trsmettere               *
020300060810      **?__________________________________________________________________ */
020400060810     C     tivgd         Begsr
020500060810      *
020600060810     c                   clear                   tivgd000
020700080403     c                   eval      vgddta = %TrimR(dpoRecord)
020800060810     c                   eval      vgdtip = '$3'
020900060810     c                   eval      vgdksu = '0DPD0OUT'
021000060810     c                   eval      vgdtsc = 'WW'
021100060810     c                   eval      vgdpgm = 'FIEU42R'
021200061023     c                   eval      vgddat = udtymd
021300060810      *
021400060810     C                   WRITE     tivgd000
021500060810      *
021600060810     C                   Endsr
021700060810      *----------------------------------------------------*
