000100000725     H DECEDIT('0,') DATEDIT(*DMY.)
000200070126      * Fidg13r *----------------------------------------------------*
000300070129      *      lista pianificazione giri x autotrasp.
000400911204      *--------------------------------------------------------------*
000500070126     FFidg13D   CF   E             WORKSTN
000600060927     ffiapd01l  if   e           k disk
000700060927     fazorg01l  if   e           k disk
000800940809     D*
000900070510     ddtaeur           s               d   datfmt(*eur)
001000070510     ddtaiso_Amg       s               d
001100061002     ddtaiso           s               d
001200070725     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001300950314RM*  D L1              S              3  0 DIM(30) ASCEND
001400950314RM*  D L6              S              3  0 DIM(30) ASCEND
001500060927     d* Parametri x Controllo profilo utenti
001600070119     d TIBS34ds      e ds
001700060203     d AZUTEds       e ds                  extname(AZUTE00F)
001800060203     d DDatiUte      e ds
001801070725     d og148         e ds
001802070725     d og147         e ds
001900060927     d*
002000060927     D KPJBA         E DS
002100060926     D fnlv24ds      E DS
002200070124     D fidg09ds      E DS
002300070126     D fidg14ds      E DS
002400070119     d dapdflr       e ds
002500940909     D* DS PER TRUL06R - CARICAMENTO £X
002600940909     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002700060926     D  LIN                    1     90  0
002800950314     D                                     DIM(30)
002900060926     D                 DS
003000061213     D  Vpdr                   1      7
003100061213     D  Vfgs                   1      3  0
003200061213     D  Vcpdr                  4      7
003300070126     c**********************************************************************
003400070126     c                   exsr      srinz
003500060926     c*
003600070126     c                   do        *hival
003700070125     c                   WRITE     dg02T01
003800061213     C                   EXFMT     dg02D01
003900920918     C* CMD3 - FINE LAVORO
004000060926     C   KC              leave
004100060927     c                   exsr      ctr01
004200061220     c* errore
004300070126     c                   if        *in28
004400061220     c                   iter
004500061220     c                   end
004600070126     c* lancio
004700070126     c                   if        *inkf
004800070126     c                   exsr      srlancio
005000070830     c                   leave
005100070126     c                   end
005200060927     c                   enddo
005300060927     c*
005400070126     c                   seton                                        lr
005500070126     C**********************************************************************
005600070126     c     srinz         begsr
005700070126     C**********************************************************************
005800070126      *
005900070126     c                   clear                   vmsg
006000070126     c                   movea     *all'0'       *in
006100070129RM*  C                   MOVEL(p)  *all'0'       VCPdr
006200070130RM*  C                   MOVE      dutpou        Vfgs
006300070130RM*  C                   MOVE      dutpou        Vfgsg
006400070126RM*  C                   MOVE      'A'           Vcord
006600070510     c                   movel     dtaiso_Amg    vdat
006700070510      *
006800070126     c                   clear                   vdpdr
006900070126     c                   clear                   vsccgi
007000070126     c                   clear                   vsdcgi
007100070126     c                   clear                   vcain
007101070928     C     dutpou        chain     azorg01l
007102070928     c                   if        %found(azorg01l)
007103070928     C                   MOVEL     ORGde8        og148
007104070928     C                   MOVEL     ORGde7        og147
007105070928     c                   else
007106070928     c                   clear                   og148
007107070928     c                   clear                   og147
007108070928     c                   end
007109070928     c     §OGCGIO       comp      ' '                                    51
007110070928     c                   if        §ogcgio = 'S'
007111070928RM*  C                   MOVE      'E'           Vctp
007112070928     c                   else
007113070928RM*  C                   MOVE      'C'           Vctp
007114070928     c                   end
007200070126     c                   endsr
007300060927     C**********************************************************************
007400070124     c     srriccgi      begsr
007500060927     C**********************************************************************
007600070112     c*
007700070124     c                   setoff                                       08
007800070124     c                   clear                   fidg09ds
007900070124     c                   eval      D09IOP0 = 'P01'
008000070129     c                   eval      d09ifgs = vfgsg
008100070126     c                   move      vdat          dtaiso
008200070126     c                   move      dtaiso        d09idat
008300070124     c                   eval      d09ides = vsdcgi
008400070723     c                   eval      d09itug = vctp
008401070723     c                   if        vctp = 'E'
008402070723     c                   clear                   d09itug
008403070723     c                   end
008500070124     c                   eval      kpjbu = fidg09ds
008600070124     c                   call      'FIDG09R'
008700070124     c                   parm                    kpjba
008800070124     c                   eval      fidg09ds = kpjbu
008900070124     c                   if        d09oerr = '1'
009000070124     c                   seton                                        2808
009100070129     c                   movel     Msg(1)        Vmsg
009200070124     c                   else
009300070130     c                   if        d09ocgi <> ' '
009400070124     c                   eval      vsccgi = d09ocgi
009500070124     c                   eval      vsdcgi = d09odes
009600070124     c                   end
009700070130     c                   end
009800070124     c*
009900070124     c                   endsr
010000070112     C**********************************************************************
010100070112     c     ctr01         begsr
010200070112     C**********************************************************************
010300070129RM*  C                   SETOFF                                       280607
010400070725RM*  C                   SETOFF                                       080504
010500061220     c                   clear                   vmsg
010600060927     c                   do
010700070126     C** ricerca autotrasportatore
010800061213RM*  C     '?'           SCAN      Vcpdr                                  31
010900060926     C                   if        *in31
011000060926     C                   clear                   fnlv24ds
011100060926     C                   MOVEL     tipo          d24tip
011200061213     C                   MOVEL     vfgs          d24fil
011300060926     C                   MOVEL     'R'           d24FLG
011400060926     C                   MOVEL(p)  fnlv24ds      KPJBU
011500060926     C                   CALL      'FNLV24R'
011600060926     C                   PARM                    KPJBA
011700060926     C                   MOVEL     KPJBU         fnlv24ds
011800061213     c                   move      d24pdr        vpdr
011900061213     c                   movel     d24rsc        vdpdr
012000060926     c                   end
012100070725     C* Filiale diversa da 0:
012200070725    2C     Vfgs          IFne      *ZEROS
012201070725     C     Vfgs          chain     azorg01l
012202070725     c                   if        %found(azorg01l)
012203070725     C                   MOVEL     ORGde8        og148
012204070725     C                   MOVEL     ORGde7        og147
012205070725     c                   else
012206070725     c                   clear                   og148
012207070725     c                   clear                   og147
012208070725     C                   MOVEL     MSG(2)        VMSG
012209070725     C                   SETON                                        2806
012210070725     C                   leave
012211070725     c                   end
012300030129     c                   select
012400060203     c                   when      DUTlpo = '2' or DUTlpo = *blanks
012500070725     C* deve essere in £6 se secondo livello
012600070725     C     Vfgs          LOOKUP    L6                                     45
012700070725     C                   when      DUTlpo = '1'
012800070725     C* deve essere in £1 se primo livello
012900070725     C     Vfgs          LOOKUP    L1                                     45
013000070725     C                   when      DUTlpo = 'S'
013100070725     C* deve essere in azorg01l
013400030203     c                   if        orgfag <> 'A' and orgfag <> 'F'
013500070725     c                   setoff                                       45
013600030203     c                   else
013700030129     c                   seton                                        45
013800030203     c                   end
014200070129     C                   ENDsl
014300070129     C* Filiale non presente in tabella
014400070129     C     *IN45         IFEQ      *OFF
014500070129     C                   MOVEL     MSG(2)        VMSG
014600070129     C                   SETON                                        2806
014700070129     C                   leave
014800070129     C                   END
014900070129     C                   END
014901070725     c* controllo se filiale abilitata a consegne e ritiri
014902070725     c                   if        vfgs <> 0 and (
014903070725     c                             (§OGCGIO = ' ' and vctp = 'R') or
014904070725     c                             (§OGTLLS = ' ' and vctp = 'C'))
014905070725     C                   MOVEL     MSG(4)        VMSG
014906070725     C                   SETON                                        04  28
014907070725     c                   leave
014908070725     c                   end
015000070126     C* controllo autotrasportatore
015100070129     c                   if        vcpdr <> ' ' and vcpdr <> '0000'
015200070129     C                   TESTN                   Vpdr                 50
015300061213RM*  C                   MOVE      Vpdr          W001A             1
015400060926RM*  C     *IN50         IFEQ      *OFF
015500060926RM*  C     W001A         ORLT      '0'
015600070129     C                   MOVEL     MSG(1)        VMSG
015700070129     C                   SETON                                        07  28
015800060927     C                   leave
015900060926RM*  C                   END
016000061213     c                   move      vpdr          kpdr
016100060926     c     kapd          chain     fiapd01l
016200060926     c                   if        %found(fiapd01l)
016300061213     c                   movel     apdrsc        vdpdr
016400070119     c                   movel     apdflr        dapdflr
016500070119     c* l'autotrasportatore non gestisce i giri
016600070207     c                   if        §apdfcgi <>'S'
016700070129     C                   MOVEL     MSG(3)        VMSG
016800070129     C                   SETON                                        07  28
016900070119     c                   leave
017000070119     c                   end
017100061220     c                   else
017200070129     C                   MOVEL     MSG(1)        VMSG
017300070129     C                   SETON                                        07  28
017400061220     c                   leave
017500070119     c                   end
017600070129     c                   end
017700070129RM*  C* Filiale diversa da 0:
017800070130     c                   eval      vfgsg=vfgs
017900070129RM* 2C     Vfgsg         IFne      *ZEROS
018000070129     c                   select
018100070129     c                   when      DUTlpo = '2' or DUTlpo = *blanks
018200070129RM*  C* deve essere in £6 se secondo livello
018300070129RM*  C     Vfgsg         LOOKUP    L6                                     45
018400070129RM*  C                   when      DUTlpo = '1'
018500070129RM*  C* deve essere in £1 se primo livello
018600070129RM*  C     Vfgsg         LOOKUP    L1                                     45
018700070129RM*  C                   when      DUTlpo = 'S'
018800070129RM*  C* deve essere in azorg01l
018900070129RM*  C     Vfgsg         chain     azorg01l
019000070129     c                   if        %found(azorg01l)
019100070129     c                   if        orgfag <> 'A' and orgfag <> 'F'
019200070129     c                   setoff                                       45
019300070129     c                   else
019400070129     c                   seton                                        45
019500070129     c                   end
019600070129     c                   else
019700070129     c                   setoff                                       45
019800070129     c                   end
019900070129     C                   ENDsl
020000070129     C* Filiale non presente in tabella
020100070129     C     *IN45         IFEQ      *OFF
020200070129     C                   MOVEL     MSG(2)        VMSG
020300070129     C                   SETON                                        2805
020400070129     C                   END
020500070129     C                   END
020600070126     c* giro
020700070124RM*  C     '?'           SCAN      Vsccgi                                 31
020800070124     c                   if        *in31
020900070124     C                   EXSR      srriccgi
021000070124     c                   end
021100070126     c                   if        vsccgi <> ' '
021200070124     C                   EXSR      srcgi
021300070126     c                   else
021400070126     c                   clear                   vsdcgi
021500070126     c                   end
021600070126     C*
021700061218    1C                   ENDDO
021800061218     C*
021900061218     C                   ENDSR
022000940809     C**************************************************************************
022100070124     C* controllo e decodifico giro
022200940809     C**************************************************************************
022300070124     C     Srcgi         BEGSR
022400940809     C*
022500070130     c                   setoff                                       0805
022600070130     c                   clear                   fidg09ds
022700070130     c                   eval      D09IOP0 = '001'
022800070130     c                   eval      d09ifgs = vfgsg
022900070130     c                   eval      d09icgi = vsccgi
023000070130     c*                  if        vsccgi = ' '
023100070130     c*                  eval      d09ides = vsdcgi
023200070130     c*                  end
023300070130     c                   move      vdat          dtaiso
023400070130     c                   move      dtaiso        d09idat
023501070829     c                   eval      d09itug = vctp
023502070829     c                   if        vctp = 'E'
023503070829     c                   clear                   d09itug
023504070829     c                   end
023600070130     c                   eval      kpjbu = fidg09ds
023700070130     c                   call      'FIDG09R'
023800070130     c                   parm                    kpjba
023900070130     c                   eval      fidg09ds = kpjbu
024000070130     c                   if        d09oerr = '1'
024100070130     c                   seton                                        280805
024200070130     c                   movel     Msg(1)        Vmsg
024300070130     c                   else
024400070130     c                   if        d09ocgi <> ' '
024500070130     c                   eval      vfgsg= d09ofgs
024600070130     c                   eval      vsccgi = d09ocgi
024700070130     c                   eval      vsdcgi = d09odes
024800070130     c                   end
024900070130     c                   end
025000070124     C*
025100070124     C                   ENDSR
025200060926      *---------------------------------------------------------------*
025300070126      * lancio
025400060926      *---------------------------------------------------------------*
025500070126     c     srlancio      BEGSR
025600070126     C*
025700070126     c                   clear                   fidg14ds
025800070126     c                   eval      d14fgs = vfgs
025900070202     c                   if        vcpdr <> '0000' and vcpdr <> ' '
026000070202     c                   move      vpdr          d14pdr
026100070202     c                   end
026200070129     c                   eval      d14fgsg= vfgsg
026300070126     c                   eval      d14cgi = vsccgi
026400070129     c                   eval      d14ain = vcain
026401070723     c                   eval      d14tp = vctp
026500070126     c                   eval      d14ordine = vcord
026600070126     c                   move      vdat          dtaiso
026700070126     c                   move      dtaiso        d14dat
026800071009     c                   eval      kpjbu = fidg14ds
026900070129     c     knmus         ifeq      *all'1'
027000070129     c                   call      'FIDG14R'
027100070129     c                   parm                    kpjba
027200070129     c                   else
027300070129     C                   MOVEL     'DG14'        KCOAZ
027400070129     C                   CALL      'BCH10'
027500070129     C                   PARM                    KPJBA
027600070129     c                   end
027700070126     C*
027800070126     C                   ENDSR
027900070126      *---------------------------------------------------------------*
028000070126      * Reperimento Dati del job (Utente/Operativi)                   *
028100070126      *---------------------------------------------------------------*
028200070126     c     DatiJob       BEGSR
028300060926      *
028400060926     c     *dtaara       define    §azute        azuteds
028500060926     c     *dtaara       define    §datiute      ddatiute
028600060926      *
028700060926     c                   in(E)     *dtaara
028800060926     c                   IF        %ERROR or RSUT = *blanks
028900060926     c                   clear                   Tibs34Ds
029000060926     c                   call      'TIBS34R'
029100060926     c                   parm                    Tibs34Ds
029200060926     c                   in        *dtaara
029300060926     c                   ENDIF
029400060926      *
029500060926     c                   ENDSR
029600940809     C**************************************************************************
029700940809     C*     R O U T I N E     I N I Z I A L E
029800940809     C**************************************************************************
029900940809     C     *INZSR        BEGSR
030000060926     c*
030100060926     C     *ENTRY        PLIST
030200060926     C                   PARM                    KPJBA
030300070510      *
030400070510     C                   TIME                    W0140            14 0
030500070510     C                   MOVE      W0140         UDATE8            8 0
030600070510      *
030700070510     c                   move      udate8        dateu8            8 0
030800070510     c                   move      udate8        dtaeur
030900070510     c                   move      dtaeur        dtaiso_Amg
031000070510     c                   move      dtaiso_Amg    udate_8           8 0
031100070510      *
031200060926      * Reperisco dati job
031300060926     c                   exsr      DatiJob
031400070126     C                   MOVEL     RSUT          VDSA
031500060928     C* CARICO LA TABELLA '£1'
031600060928     C                   MOVE      '£1'          D06COD
031700060928     C                   MOVEL     SIMFEL        D06KEY
031800060928     C                   MOVEL     DSUL06        KPJBU
031900060928     C                   CALL      'TRUL06R'
032000060928     C                   PARM                    KPJBA
032100060928     C                   MOVEL     KPJBU         DSUL06
032200060928     C                   MOVEA     LIN           L1
032300060928RM*  C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
032400060928     c                   if            DUTlpo = '2'
032500060928     c                             or  DUTlpo = *blanks
032600060928RM*  C                   MOVE      '£6'          D06COD
032700060928RM*  c                   movel     DUTpou        D06KEY
032800060928RM*  C                   MOVEL     DSUL06        KPJBU
032900060928RM*  C                   CALL      'TRUL06R'
033000060928RM*  C                   PARM                    KPJBA
033100060928RM*  C                   MOVEL     KPJBU         DSUL06
033200060928RM*  C                   MOVEA     LIN           L6
033300060928RM*  C                   END
033400940809     C*
033500940809     C     *LIKE         DEFINE    APDPDR        KPDR
033600940909     C*
033700021129     C     Kapd          KLIST
033800060927     C                   KFLD                    tipo              1
033900060927     C                   KFLD                    kpdr              7 0
034000060927     c                   eval      tipo = 'A'
034100010207     C*
034200010207     C                   Z-ADD     1             KUTET             1 0
034300000808     C*
034400940809     C                   ENDSR
034500940809     C**************************************************************************
034600940809**
034700070129Immettere un codice valido                                                     1
034800070129Codice filiale errata o non gestita                                            2
034900070129L'autotrasportatore non gestisce i giri                                        3
035000070725Tipo pianificazione non abilitato                                              4
