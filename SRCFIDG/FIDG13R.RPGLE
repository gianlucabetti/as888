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
001900070725     d og148         e ds
002000070725     d og147         e ds
002100060927     d*
002200060927     D KPJBA         E DS
002300060926     D fnlv24ds      E DS
002400070124     D fidg09ds      E DS
002500070126     D fidg14ds      E DS
002600070119     d dapdflr       e ds
002700940909     D* DS PER TRUL06R - CARICAMENTO £X
002800940909     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002900060926     D  LIN                    1     90  0
003000950314     D                                     DIM(30)
003100060926     D                 DS
003200061213     D  Vpdr                   1      7
003300061213     D  Vfgs                   1      3  0
003400061213     D  Vcpdr                  4      7
003500170620
003600170620     d sav_kpjbu       s                   like(kpjbu)
003700170620
003800170620      // - Reperimento filiali gestite dall'utente
003900170620     d TRUL31DS      e ds
004000170620     d  POG                   10    759    dim(250)
004100170620
004200170620      //---------------------------------------------------------------
004300170620      //?Definizione Procedure usate.
004400170620      //---------------------------------------------------------------
004500170620       // - Caricamento Filiali in gestione
004600170620     d TRUL31R         pr                  extpgm('TRUL31R')
004700170620     d  kpjba                              likeds(kpjba)
004800170620     d  trul31ds                           likeds(trul31ds)
004900170620
005000070126     c**********************************************************************
005100070126     c                   exsr      srinz
005200060926     c*
005300070126     c                   do        *hival
005400070125     c                   WRITE     dg02T01
005500061213     C                   EXFMT     dg02D01
005600920918     C* CMD3 - FINE LAVORO
005700060926     C   KC              leave
005800060927     c                   exsr      ctr01
005900061220     c* errore
006000070126     c                   if        *in28
006100061220     c                   iter
006200061220     c                   end
006300070126     c* lancio
006400070126     c                   if        *inkf
006500070126     c                   exsr      srlancio
006600070830     c                   leave
006700070126     c                   end
006800060927     c                   enddo
006900060927     c*
007000070126     c                   seton                                        lr
007100070126     C**********************************************************************
007200070126     c     srinz         begsr
007300070126     C**********************************************************************
007400070126      *
007500070126     c                   clear                   vmsg
007600070126     c                   movea     *all'0'       *in
007700070129RM*  C                   MOVEL(p)  *all'0'       VCPdr
007800070130RM*  C                   MOVE      dutpou        Vfgs
007900070130RM*  C                   MOVE      dutpou        Vfgsg
008000070126RM*  C                   MOVE      'A'           Vcord
008100070510     c                   movel     dtaiso_Amg    vdat
008200070510      *
008300070126     c                   clear                   vdpdr
008400070126     c                   clear                   vsccgi
008500070126     c                   clear                   vsdcgi
008600070126     c                   clear                   vcain
008700070928     C     dutpou        chain     azorg01l
008800070928     c                   if        %found(azorg01l)
008900070928     C                   MOVEL     ORGde8        og148
009000070928     C                   MOVEL     ORGde7        og147
009100070928     c                   else
009200070928     c                   clear                   og148
009300070928     c                   clear                   og147
009400070928     c                   end
009500070928     c     §OGCGIO       comp      ' '                                    51
009600070928     c                   if        §ogcgio = 'S'
009700070928RM*  C                   MOVE      'E'           Vctp
009800070928     c                   else
009900070928RM*  C                   MOVE      'C'           Vctp
010000070928     c                   end
010100070126     c                   endsr
010200060927     C**********************************************************************
010300070124     c     srriccgi      begsr
010400060927     C**********************************************************************
010500070112     c*
010600070124     c                   setoff                                       08
010700070124     c                   clear                   fidg09ds
010800070124     c                   eval      D09IOP0 = 'P01'
010900070129     c                   eval      d09ifgs = vfgsg
011000070126     c                   move      vdat          dtaiso
011100070126     c                   move      dtaiso        d09idat
011200070124     c                   eval      d09ides = vsdcgi
011300070723     c                   eval      d09itug = vctp
011400070723     c                   if        vctp = 'E'
011500070723     c                   clear                   d09itug
011600070723     c                   end
011700070124     c                   eval      kpjbu = fidg09ds
011800070124     c                   call      'FIDG09R'
011900070124     c                   parm                    kpjba
012000070124     c                   eval      fidg09ds = kpjbu
012100070124     c                   if        d09oerr = '1'
012200070124     c                   seton                                        2808
012300070129     c                   movel     Msg(1)        Vmsg
012400070124     c                   else
012500070130     c                   if        d09ocgi <> ' '
012600070124     c                   eval      vsccgi = d09ocgi
012700070124     c                   eval      vsdcgi = d09odes
012800070124     c                   end
012900070130     c                   end
013000070124     c*
013100070124     c                   endsr
013200070112     C**********************************************************************
013300070112     c     ctr01         begsr
013400070112     C**********************************************************************
013500070129RM*  C                   SETOFF                                       280607
013600070725RM*  C                   SETOFF                                       080504
013700061220     c                   clear                   vmsg
013800060927     c                   do
013900070126     C** ricerca autotrasportatore
014000061213RM*  C     '?'           SCAN      Vcpdr                                  31
014100060926     C                   if        *in31
014200060926     C                   clear                   fnlv24ds
014300060926     C                   MOVEL     tipo          d24tip
014400061213     C                   MOVEL     vfgs          d24fil
014500060926     C                   MOVEL     'R'           d24FLG
014600060926     C                   MOVEL(p)  fnlv24ds      KPJBU
014700060926     C                   CALL      'FNLV24R'
014800060926     C                   PARM                    KPJBA
014900060926     C                   MOVEL     KPJBU         fnlv24ds
015000061213     c                   move      d24pdr        vpdr
015100061213     c                   movel     d24rsc        vdpdr
015200060926     c                   end
015300070725     C* Filiale diversa da 0:
015400070725    2C     Vfgs          IFne      *ZEROS
015500070725     C     Vfgs          chain     azorg01l
015600070725     c                   if        %found(azorg01l)
015700070725     C                   MOVEL     ORGde8        og148
015800070725     C                   MOVEL     ORGde7        og147
015900070725     c                   else
016000070725     c                   clear                   og148
016100070725     c                   clear                   og147
016200070725     C                   MOVEL     MSG(2)        VMSG
016300070725     C                   SETON                                        2806
016400070725     C                   leave
016500070725     c                   end
016600170620     c*****              select
016700170620     c*****              when      DUTlpo = '2' or DUTlpo = *blanks
016800070725     C* deve essere in £6 se secondo livello
016900170620     C*****Vfgs          LOOKUP    L6                                     45
017000170620     C*****              when      DUTlpo = '1'
017100070725     C* deve essere in £1 se primo livello
017200170620     C*****Vfgs          LOOKUP    L1                                     45
017300170620     C*****              when      DUTlpo = 'S'
017400070725     C* deve essere in azorg01l
017500030203     c                   if        orgfag <> 'A' and orgfag <> 'F'
017600070725     c                   setoff                                       45
017700030203     c                   else
017800030129     c                   seton                                        45
017900030203     c                   end
018000170620     C*****              ENDsl
018100170620
018200170620       //?Filiale in gestione all'utente
018300170620         IF  %lookup(%editc(vfgs:'X'):pog) = 0;
018400170620           *in45 = *off;
018500170620         ENDIF;
018600170620
018700070129     C* Filiale non presente in tabella
018800070129     C     *IN45         IFEQ      *OFF
018900070129     C                   MOVEL     MSG(2)        VMSG
019000070129     C                   SETON                                        2806
019100070129     C                   leave
019200070129     C                   END
019300070129     C                   END
019400070725     c* controllo se filiale abilitata a consegne e ritiri
019500070725     c                   if        vfgs <> 0 and (
019600070725     c                             (§OGCGIO = ' ' and vctp = 'R') or
019700070725     c                             (§OGTLLS = ' ' and vctp = 'C'))
019800070725     C                   MOVEL     MSG(4)        VMSG
019900070725     C                   SETON                                        04  28
020000070725     c                   leave
020100070725     c                   end
020200070126     C* controllo autotrasportatore
020300070129     c                   if        vcpdr <> ' ' and vcpdr <> '0000'
020400070129     C                   TESTN                   Vpdr                 50
020500061213RM*  C                   MOVE      Vpdr          W001A             1
020600060926RM*  C     *IN50         IFEQ      *OFF
020700060926RM*  C     W001A         ORLT      '0'
020800070129     C                   MOVEL     MSG(1)        VMSG
020900070129     C                   SETON                                        07  28
021000060927     C                   leave
021100060926RM*  C                   END
021200061213     c                   move      vpdr          kpdr
021300060926     c     kapd          chain     fiapd01l
021400060926     c                   if        %found(fiapd01l)
021500061213     c                   movel     apdrsc        vdpdr
021600070119     c                   movel     apdflr        dapdflr
021700070119     c* l'autotrasportatore non gestisce i giri
021800170620     c*****              if        §apdfcgi <>'S'
021900170620     C*****              MOVEL     MSG(3)        VMSG
022000170620     C*****              SETON                                        07  28
022100170620     c*****              leave
022200170620     c*****              end
022300061220     c                   else
022400070129     C                   MOVEL     MSG(1)        VMSG
022500070129     C                   SETON                                        07  28
022600061220     c                   leave
022700070119     c                   end
022800070129     c                   end
022900070129RM*  C* Filiale diversa da 0:
023000070130     c                   eval      vfgsg=vfgs
023100070129RM* 2C     Vfgsg         IFne      *ZEROS
023200170620     c*****              select
023300170620     c*****              when      DUTlpo = '2' or DUTlpo = *blanks
023400070129RM*  C* deve essere in £6 se secondo livello
023500170620RM*  C*****Vfgsg         LOOKUP    L6                                     45
023600170620RM*  C*****              when      DUTlpo = '1'
023700070129RM*  C* deve essere in £1 se primo livello
023800170620RM*  C*****Vfgsg         LOOKUP    L1                                     45
023900170620RM*  C*****              when      DUTlpo = 'S'
024000070129RM*  C* deve essere in azorg01l
024100070129RM*  C     Vfgsg         chain     azorg01l
024200070129     c                   if        %found(azorg01l)
024300070129     c                   if        orgfag <> 'A' and orgfag <> 'F'
024400070129     c                   setoff                                       45
024500070129     c                   else
024600070129     c                   seton                                        45
024700070129     c                   end
024800070129     c                   else
024900070129     c                   setoff                                       45
025000070129     c                   end
025100170620     C*****              ENDsl
025200170620
025300170620       //?Filiale in gestione all'utente
025400170620         IF  %lookup(%editc(vfgsg:'X'):pog) = 0;
025500170620           *in45 = *off;
025600170620         ENDIF;
025700170620
025800070129     C* Filiale non presente in tabella
025900070129     C     *IN45         IFEQ      *OFF
026000070129     C                   MOVEL     MSG(2)        VMSG
026100070129     C                   SETON                                        2805
026200070129     C                   END
026300070129     C                   END
026400070126     c* giro
026500070124RM*  C     '?'           SCAN      Vsccgi                                 31
026600070124     c                   if        *in31
026700070124     C                   EXSR      srriccgi
026800070124     c                   end
026900070126     c                   if        vsccgi <> ' '
027000070124     C                   EXSR      srcgi
027100070126     c                   else
027200070126     c                   clear                   vsdcgi
027300070126     c                   end
027400070126     C*
027500061218    1C                   ENDDO
027600061218     C*
027700061218     C                   ENDSR
027800940809     C**************************************************************************
027900070124     C* controllo e decodifico giro
028000940809     C**************************************************************************
028100070124     C     Srcgi         BEGSR
028200940809     C*
028300070130     c                   setoff                                       0805
028400070130     c                   clear                   fidg09ds
028500070130     c                   eval      D09IOP0 = '001'
028600070130     c                   eval      d09ifgs = vfgsg
028700070130     c                   eval      d09icgi = vsccgi
028800070130     c*                  if        vsccgi = ' '
028900070130     c*                  eval      d09ides = vsdcgi
029000070130     c*                  end
029100070130     c                   move      vdat          dtaiso
029200070130     c                   move      dtaiso        d09idat
029300070829     c                   eval      d09itug = vctp
029400070829     c                   if        vctp = 'E'
029500070829     c                   clear                   d09itug
029600070829     c                   end
029700070130     c                   eval      kpjbu = fidg09ds
029800070130     c                   call      'FIDG09R'
029900070130     c                   parm                    kpjba
030000070130     c                   eval      fidg09ds = kpjbu
030100070130     c                   if        d09oerr = '1'
030200070130     c                   seton                                        280805
030300070130     c                   movel     Msg(1)        Vmsg
030400070130     c                   else
030500070130     c                   if        d09ocgi <> ' '
030600070130     c                   eval      vfgsg= d09ofgs
030700070130     c                   eval      vsccgi = d09ocgi
030800070130     c                   eval      vsdcgi = d09odes
030900070130     c                   end
031000070130     c                   end
031100070124     C*
031200070124     C                   ENDSR
031300060926      *---------------------------------------------------------------*
031400070126      * lancio
031500060926      *---------------------------------------------------------------*
031600070126     c     srlancio      BEGSR
031700070126     C*
031800070126     c                   clear                   fidg14ds
031900070126     c                   eval      d14fgs = vfgs
032000070202     c                   if        vcpdr <> '0000' and vcpdr <> ' '
032100070202     c                   move      vpdr          d14pdr
032200070202     c                   end
032300070129     c                   eval      d14fgsg= vfgsg
032400070126     c                   eval      d14cgi = vsccgi
032500070129     c                   eval      d14ain = vcain
032600070723     c                   eval      d14tp = vctp
032700070126     c                   eval      d14ordine = vcord
032800070126     c                   move      vdat          dtaiso
032900070126     c                   move      dtaiso        d14dat
033000071009     c                   eval      kpjbu = fidg14ds
033100070129     c     knmus         ifeq      *all'1'
033200070129     c                   call      'FIDG14R'
033300070129     c                   parm                    kpjba
033400070129     c                   else
033500070129     C                   MOVEL     'DG14'        KCOAZ
033600070129     C                   CALL      'BCH10'
033700070129     C                   PARM                    KPJBA
033800070129     c                   end
033900070126     C*
034000070126     C                   ENDSR
034100070126      *---------------------------------------------------------------*
034200070126      * Reperimento Dati del job (Utente/Operativi)                   *
034300070126      *---------------------------------------------------------------*
034400070126     c     DatiJob       BEGSR
034500060926      *
034600060926     c     *dtaara       define    §azute        azuteds
034700060926     c     *dtaara       define    §datiute      ddatiute
034800060926      *
034900060926     c                   in(E)     *dtaara
035000060926     c                   IF        %ERROR or RSUT = *blanks
035100060926     c                   clear                   Tibs34Ds
035200060926     c                   call      'TIBS34R'
035300060926     c                   parm                    Tibs34Ds
035400060926     c                   in        *dtaara
035500060926     c                   ENDIF
035600170620
035700170620      // Reperimento delle filiali gestibili dall'utente
035800170620        sav_kpjbu = kpjbu;
035900170620        clear kpjbu;
036000170620        clear TRUL31DS;
036100170620        IF  DUTlpo = '2';
036200170620          I31abi = 'PO';
036300170620        ELSE;
036400170620          I31abi = 'RA';
036500170620        ENDIF;
036600170620        I31cdi = DUTdis;
036700170620        I31car = DUTare;
036800170620        I31cpo = DUTpou;
036900170620        trul31r (kpjba:trul31ds);
037000170620        IF  O31pog <= *zeros;
037100170620          clear POG;
037200170620        ENDIF;
037300170620        kpjbu = sav_kpjbu;
037400060926      *
037500060926     c                   ENDSR
037600940809     C**************************************************************************
037700940809     C*     R O U T I N E     I N I Z I A L E
037800940809     C**************************************************************************
037900940809     C     *INZSR        BEGSR
038000060926     c*
038100060926     C     *ENTRY        PLIST
038200060926     C                   PARM                    KPJBA
038300070510      *
038400070510     C                   TIME                    W0140            14 0
038500070510     C                   MOVE      W0140         UDATE8            8 0
038600070510      *
038700070510     c                   move      udate8        dateu8            8 0
038800070510     c                   move      udate8        dtaeur
038900070510     c                   move      dtaeur        dtaiso_Amg
039000070510     c                   move      dtaiso_Amg    udate_8           8 0
039100070510      *
039200060926      * Reperisco dati job
039300060926     c                   exsr      DatiJob
039400070126     C                   MOVEL     RSUT          VDSA
039500060928     C* CARICO LA TABELLA '£1'
039600060928     C                   MOVE      '£1'          D06COD
039700060928     C                   MOVEL     SIMFEL        D06KEY
039800060928     C                   MOVEL     DSUL06        KPJBU
039900060928     C                   CALL      'TRUL06R'
040000060928     C                   PARM                    KPJBA
040100060928     C                   MOVEL     KPJBU         DSUL06
040200060928     C                   MOVEA     LIN           L1
040300060928RM*  C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
040400060928     c                   if            DUTlpo = '2'
040500060928     c                             or  DUTlpo = *blanks
040600060928RM*  C                   MOVE      '£6'          D06COD
040700060928RM*  c                   movel     DUTpou        D06KEY
040800060928RM*  C                   MOVEL     DSUL06        KPJBU
040900060928RM*  C                   CALL      'TRUL06R'
041000060928RM*  C                   PARM                    KPJBA
041100060928RM*  C                   MOVEL     KPJBU         DSUL06
041200060928RM*  C                   MOVEA     LIN           L6
041300060928RM*  C                   END
041400940809     C*
041500940809     C     *LIKE         DEFINE    APDPDR        KPDR
041600940909     C*
041700021129     C     Kapd          KLIST
041800060927     C                   KFLD                    tipo              1
041900060927     C                   KFLD                    kpdr              7 0
042000060927     c                   eval      tipo = 'A'
042100010207     C*
042200010207     C                   Z-ADD     1             KUTET             1 0
042300000808     C*
042400940809     C                   ENDSR
042500940809     C**************************************************************************
042600940809**
042700070129Immettere un codice valido                                                     1
042800070129Codice filiale errata o non gestita                                            2
042900070129L'autotrasportatore non gestisce i giri                                        3
043000070725Tipo pianificazione non abilitato                                              4
