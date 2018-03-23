000100051019      *PARMS CLOSQLCSR(*ENDMOD)
000200051019      *PARMS DYNUSRPRF(*OWNER)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400051019      * FNLss5R *----------------------------------------------------*
000500051019      *        - Stampa segnacolli export dpd in partenza
000600911205      *--------------------------------------------------------------*
000700970715     FAZORG01L  IF   E           K DISK
000800051019     FFNblp00f  IF   E             DISK
000900051020     FFNblp01l  IF   E           K DISK    usropn
001000051020     F                                     rename(fnblp000:fnblp01)
001100060220     FFiar401L  IF   E           K DISK
001200971219     FTABEL00F  IF   E           K DISK
001300060616     Fdpcdp01l  IF   E           K DISK
001400060616     Fdpcsc01l  IF   E           K DISK
001500050418     fFIAR501L  if   e           k disk    usropn
001600970715     FFNLV22P   O    F  132        PRINTER
001700980330     D*
001800911205      *
001900051020     D KCOD            S                   LIKE(TBLCOD) inz
002000051020     D KKEY            S                   LIKE(TBLKEY) inz
002100051020     D $CBLPR          S                   LIKE(BLPCBO) inz
002200051019     d StringaSql      s            500    Varying
002300051019     D fnlss4ds      E DS
002400051019     D fnblpxxds     E DS                  extname(fnblp00f)
002500051020     D pog             S              3  0 DIM(30)
002600051020     d Azuteds       e ds                  Extname(Azute00f)
002700051020     d dDatiute      e ds
002800051020     d Tibs34ds      e ds                  Inz
002900060616     d tisie2ds      e ds
003000060616     d trul28ds4     e ds
003100051020      * ds per trul06r
003200051020     d trul06ds      e ds
003300051020     d  lin                    1     90  0 dim(30)
003400911211      *
003500000203     D                 DS
003600000203     D  DPDPAR                 1     12
003700000203     D  DPD1                   1      3
003800000203     D  DPD2                   4     11
003900000203     D                 DS
004000000203     D  DPDSTP                 1     14
004100000203     D  DPDS1                  1      3
004200000203     D  DPDV1                  4      4    INZ
004300000203     D  DPDS2                  5     12
004400000203     D  DPDV2                 13     13    INZ
004500000203     D  DPDS3                 14     14
004600000203     D* DS PER TIBS02R - GESTIONE TNTBE00F
004700000203     D DSBS02        E DS                  EXTNAME(TIBS02DS)
004800030613      *
004900060519     D*DSBL4F        E DS
005000060519     D DSBL4I        E DS
005100970715     D DSBLD         E DS
005200971219     D DSBLDE        E DS
005300000201     D OG143         E DS
005400040518     D OG150         E DS
005500971219     D DS15          E DS
005600971219     D DS3A          E DS
005700940216     D KPJBA         E DS
005800031028     d dsbl4a        e ds
005900050418     d dAR5gen       e ds                  inz
006000060519     d w005a           s              5
006100060620     d w016a           s             16
006200060616     d wcalltisie2     s              1
006300060616     d ksrvc           s                   like(cscsrvc)
006400911206      ****************************************************************
006500911206      *  RIEPILOGO INDICATORI
006600911210      ****************************************************************
006700911206      *****************************************************************
006800000000     C     *ENTRY        PLIST
006900000000     C                   PARM                    KPJBA
007000051019     C                   MOVEL     KPJBU         fnlss4ds
007100051020     c*
007200051020     c     *dtaara       define    §azute        azuteds
007300051020     c     *dtaara       define    §datiute      ddatiute
007400051020     c                   in(E)     *dtaara
007500051020     c                   If        %error  or RSUT = *blanks
007600051020     c                   CLEAR                   tibs34ds
007700051020     c                   CALL      'TIBS34R'
007800051020     c                   PARM                    tibs34ds
007900051020     c                   in        *dtaara
008000051020     c                   EndIf
008100911211      *---------------------------------------------------------------*
008200911211      * CHIAVE X POSIZIONAMENTO SU FLARB35C
008300911211      *
008400060220     C     Kar42         KLIST
008500051019     C                   KFLD                    blpAAS
008600051019     C                   KFLD                    blpLNP
008700051019     C                   KFLD                    blpNRS
008800051019     C                   KFLD                    blpNSP
008900970203     C                   KFLD                    WTRC
009000051020     C     Kblp          KLIST
009100051020     C                   KFLD                    is4saa
009200051020     C                   KFLD                    is4sln
009300051020     C                   KFLD                    is4ssr
009400051020     C                   KFLD                    is4ssp
009500051020     C* Chiave di accesso per il file tabelle
009600051020     C     KTAB1         KLIST
009700051020     C                   KFLD                    CODUT
009800051020     C                   KFLD                    KCOD
009900051020     C                   KFLD                    KKEY
010000971219     C     KTAB2         KLIST
010100971219     C                   KFLD                    CODUT             1 0
010200971219     C                   KFLD                    COD               2
010300971219     C                   KFLD                    KEY               8
010400971219     C                   Z-ADD     1             CODUT
010500060616     C     KCDP          KLIST
010600060616     C                   KFLD                    sie2ver
010700060616     C                   KFLD                    §b4ida
010800060616     C     KCSC          KLIST
010900060616     C                   KFLD                    sie2ver
011000060616     C                   KFLD                    ksrvc
011100050418      * FIAR501L
011200050418     c     k05AR501      klist
011300051019     c                   kfld                    blpaas
011400051019     c                   kfld                    blplnp
011500051019     c                   kfld                    blpnrs
011600051019     c                   kfld                    blpnsp
011700051019     c                   kfld                    aR5trd
011800050418     c                   eval      AR5trd = 'GEN'
011900970203     C* DEFINIZIONE CAMPI
012000020207     C     *LIKE         DEFINE    §OGNTW        LNANTW
012100020207     C     *LIKE         DEFINE    §OGNTW        LNPNTW
012200060220     C     *LIKE         DEFINE    ar4TRC        WTRC
012300911206      *---------------------------------------------------------------*
012400040519
012500051019     c* Eseguo sql per estrarre da fnblp solo i dati che mi servono
012600051020     c                   if        is4drt > *zeros
012700051019     c                   exsr      sr_SqlBlp
012800051019     c* Lettura ed elaborazione di fnblp
012900051020     c                   if        is4lnp = *zeros
013000051020     c                   exsr      carpog
013100051020     c                   endif
013200051019     c                   exsr      sr_LeggiBlp
013300051020     c                   else
013400051020     c* se richiesta una singola spedizione chaino e stampo
013500051020     c                   open      fnblp01l
013600051020     c     kblp          chain     fnblp01l
013700051020     c                   if        %found(fnblp01l)
013800051021     c                   exsr      S_OrgLna
013900060220     c                   ExSr      Legar4
014000051020     c                   ExSr      StaNsc
014100051020     c                   endif
014200051020     c                   close     fnblp01l
014300051020     c                   endif
014400911206      *
014500060616     c                   if        wcalltisie2='S'
014600060616     c                   clear                   tisie2ds
014700060616     c                   eval      sie2tla='C'
014800060616     c                   call      'TISIE2R'
014900060616     c                   parm                    tisie2ds
015000060616     c                   endif
015100040519     c*
015200911206     C                   SETON                                        LR
015300051019     c**********************************************************************
015400051019     c     Sr_Sqlblp     begsr
015500051019     c                   eval      StringaSql =
015600051019     c                             'Select * '                   +
015700051019     c                             'from fnblp00f WHERE blpdrt = '       +
015800051019     c                              %editc(is4drt:'4') +
015900051019     c                             ' and ' +
016000051019     c                             ' ( ' +
016100051019     c                              %editc(is4ccm:'4') +
016200051019     c                              ' in blpksc or '   +
016300051019     c                              %editc(is4ccm:'4') +
016400051019     c                              ' in blpccm'  +
016500051020     c                             ' ) ' +
016600051019     c                             ' ORDER BY blpaas, blplnp, blpnrs, blpnsp ' +
016700051019     c                             'for fetch only'
016800051019     C/EXEC SQL
016900051019     C+ prepare S1 from :StringaSql
017000051019     C/END-EXEC
017100051019     C/EXEC SQL
017200051019     C+ declare C1 cursor for S1
017300051019     C/END-EXEC
017400051019     c                   endsr
017500051019     c**********************************************************************
017600051019     c     Sr_LeggiBlp   begsr
017700051019     c*
017800051019     c* apertura cursore
017900051019     c*
018000051019     C/EXEC SQL
018100051019     C+ Open C1
018200051019     C/END-EXEC
018300051019     c*
018400051019     c* lettura cursore
018500051019     c*
018600051019     C/EXEC SQL
018700051019     C+ Fetch C1 into :fnblpxxds
018800051019     C/END-EXEC
018900051019     c                   Dow       Sqlcod = 0
019000051019     c* ignoro spedizione di recupero
019100051019    6C     BLPCBO        IFNE      $CBLPR
019200051019     C                   MOVE      '3A'          KCOD
019300051019     C                   MOVE      *BLANKS       KKEY
019400051019     C                   MOVEL     BLPCBO        KKEY
019500051019     C     KTAB1         CHAIN     TABEL00F                           99
019600051019     C  N99              MOVEL     TBLUNI        DS3A
019700051019     C  N99              MOVE      BLPCBO        $CBLPR
019800051019     C   99              CLEAR                   DS3A
019900051019     c                   endif
020000051019     c                   if        §3arbl <> 'R'
020100051021     c                   exsr      s_OrgLna
020200051020     c* scarto il record se lna non dpd o se lna letta <> da quella scelta
020300051020     c                   if        lnantw <> 'DPD' or
020400051020     c                             (is4lna > *zeros and is4lna <> blplna)
020500051021     c                   goto      leggi
020600051020     c                   endif
020700051020     c* scarto il record se richiesto un cod.tariffa specifico ed è diverso
020800051020     c* da quello letto
020900051020     c                   if        is4ctr <> 999 and is4ctr <> blpctr
021000051021     c                   goto      leggi
021100051020     c                   endif
021200051020     c* scarto il record se lnp non gestibile dall'utente o se immessa una
021300051020     c* lnp specifica ed è diversa da quella letta
021400051020     c                   if        is4lnp > *zeros and is4lnp <> blplnp
021500051021     c                   goto      leggi
021600051020     c                   endif
021700051020     c                   if        is4lnp = *zeros
021800051020     c     blplnp        lookup    pog                                    30
021900051020     c                   if        *in30 = *off
022000051021     c                   goto      leggi
022100051020     c                   endif
022200051020     c                   endif
022300060220     c                   ExSr      Legar4
022400051019     c                   ExSr      StaNsc
022500051019     c                   endif
022600051021     c*
022700051021     c     leggi         tag
022800051019     C/EXEC SQL
022900051019     C+ Fetch C1 into :fnblpxxds
023000051019     C/END-EXEC
023100051019
023200051019     c                   EndDo
023300051019     c*
023400051019
023500051019     C/EXEC SQL
023600051019     C+ close C1
023700051019     C/END-EXEC
023800051019
023900051019     c                   endsr
024000051021     C** REPERISCO DATI DI AZORG DELLA LINEA DI ARRIVO ---------------------
024100051021     c     S_OrgLna      begsr
024200051021     C     blpLNA        IFNE      SAVLNA
024300051021     C     blpLNA        CHAIN     AZORG01L                           31
024400051021     C                   MOVE      ORGDE3        OG143
024500051021     C                   MOVEl     ORGDf0        OG150
024600051021     C                   MOVE      §OGNTW        LNANTW
024700051021     c                   clear                   dpdtel           15
024800051021     c                   clear                   dpdfax           15
024900051021     c                   movel     orgtel        dpdtel
025000051021     c                   movel     orgfax        dpdfax
025100051021     C                   Z-ADD     blplna        SAVLNA            3 0
025200051021     C                   END
025300051021     c                   endsr
025400970715     C**
025500970715     C* IMPOSTO CAMPI PER STAMPA IN IMMEDIATO ------------------------*
025600970715     C     IMPO          BEGSR
025700050418      *
025800050418      * Verifica se spedizione "di valore"
025900050418     c                   exsr      CtrSpeVal
026000970715     C*
026100970715     C                   Z-ADD     0             §LDNC1
026200051019     C                   MOVEL     blpFLS        §LDLNP
026300051019     C     blpFLS        CHAIN     AZORG                              30
026400970715     C  N30              MOVEL     ORGDES        §LDDLP
026500970715     C   30              MOVEL     *BLANKS       §LDDLP
026600051019     C                   MOVEL     blpNRS        §LDNRS
026700051019     C                   MOVEL     blpLNA        §LDLNA
026800051019     C     blpLNA        CHAIN     AZORG                              30
026900970715     C  N30ORGDE5        IFNE      *BLANKS
027000970715     C                   MOVEL     ORGDE5        §LDDLA
027100970715     C                   ELSE
027200970715     C                   MOVEL     ORGDES        §LDDLA
027300970715     C                   END
027400970715     C   30              MOVEL     *BLANKS       §LDDLA
027500051019     C                   MOVEL     blpTFA        §LDTNA
027600051019     C                   MOVEL     blpTSP        §LDTSP
027700051019     C                   MOVEL     blpZNC        §LDZSC
027800051019     C                   MOVE      blpNCL        §LDNC2
027900051019     C                   MOVEL     blpRSM        §LDRSM
028000051019     C                   MOVEL     blpPRM        §LDPRM
028100051019     C                   MOVEL     blpRSD        §LDRSD
028200051019     C                   MOVEL     blpCAD        §LDCAD
028300051019     C                   MOVEL     blpLOD        §LDLOD
028400051019     C                   MOVEL     blpIND        §LDIND
028500051019     C                   MOVEL     blpPRD        §LDPRD
028600970715     C                   MOVEL     *ZEROS        §LDNCP
028700970715     C                   MOVEL     *ZEROS        §LDNCU
028800050418if  1c                   if        §AR5bva = 'V'
028900050418     c                   movel     'X'           §LDrst
029000050418x   1c                   else
029100970715     C                   MOVEL     'R'           §LDRST
029200050418e   1c                   endif
029300051019     C                   Z-ADD     blpPKF        §LDPKG
029400051019     C                   Z-ADD     blpVLF        §LDVOL
029500051019     C                   MOVEL     blpFVF        §LDFVR
029600051019     C                   MOVEL     blpAAS        §LDAAS
029700051019     C                   MOVEL     blpMGS        §LDMGS
029800971219     C*
029900040519     c* Se sto stampando i segnacolli in arrivo in base al file di work
030000040519     c* devo stampare solo l'etichetta estera
030100040519     c                   Eval      §Detst = 'EST'
030200040519     c                   exsr      etiest
030300971219     C*
030400970715     C                   ENDSR
030500970715     C**
030600030414      **
030700030414      *--- STAMPA SEGNACOLLO -----------------------------------------*
030800030414     c     StaNsc        BegSr
030900030414
031000030414     c                   ExSr      Impo
031100030414
031200030414     c                   Add       1             §LDNC1
031300051019     c                   Movel     blpncd        §LDNCD
031400030414
031500030414
031600051020     c                   CALL      i90pss
031700030414     c                   PARM                    DSBLD
031800030414     c                   PARM                    DSBLDE
031900030414
032000030414     c                   EndSr
032100030414
032200971219     C*--- REPERISCO ESTENZIONE BOLLA TIPO RECORD 'E' ----------------*
032300060220     C     legar4        BEGSR
032400000203     C                   CLEAR                   DSBLDE
032500000203     C                   CLEAR                   DSBLD
032600971219     C*
032700051019     C                   MOVEL     blprma        §LDCDP
032800971219     C*
032900971219     C*
033000051019     C     blpLNP        CHAIN     AZORG01L                           31
033100020207     C                   MOVE      ORGDE3        OG143
033200020207     C                   MOVE      §OGNTW        LNPNTW
033300060616     C*
033400060616     C* CHAIN SULLE NAZIONI
033500060616     C                   MOVEL     '15'          COD
033600060616     C                   MOVEL(P)  blpNZD        KEY
033700060616     C     KTAB2         CHAIN     TABEL                              31
033800060616     C  N31TBLFLG        IFNE      ' '
033900060616     C                   SETON                                        31
034000060616     C                   END
034100060616     C**
034200060616     C  N31              MOVEL     TBLUNI        DS15
034300060616     C   31              CLEAR                   DS15
034400060616     C                   MOVEL     §15COD        §DEISA
034500000207     C*
034600000203      **
034700060220      **  CHAIN Fiar4 PER EXPORT DPD
034800020207    1C     LNANTW        IFEQ      'DPD'
034900060519     C                   MOVEL     'I'           WTRC
035000060220     C     Kar42         CHAIN     Fiar401L                           39
035100060519     C  N39              MOVEL     ar4NOT        DSBL4I
035200060519     C   39              CLEAR                   DSBL4I
035300060519    2C     §B4ipn        IFNE      *BLANKS
035400060616     C***                MOVEL     §B4ipn        DPDPAR
035500060616     C***                MOVEL     DPD1          DPDS1
035600060616     C***                MOVEL     DPD2          DPDS2
035700060616     C***                MOVEL     §b4icd        DPDS3
035800060616     C***                MOVEL(P)  DPDSTP        §LDCDP
035900011011     C* DEVO SEMPRE RISTAMPARE ANCHE IL CHI SONO
036000040518     c*
036100060616     c***  §ogsea        ifne      'S'
036200060616     C***                MOVEL     'DP2'         §DETSE
036300060616     c***                else
036400060616     c***                movel     'DP3'         §detse
036500060616     c***                endif
036600060616     c
036700051019     C     blpPKF        IFLE      3
036800000203     C                   MOVEL     'X'           §DESSE
036900000203     C                   ENDIF
037000060616     c                   movel     'DP4'         §detse
037100060616     c* reperisco versione cappario DPD
037200060616     c                   clear                   tisie2ds
037300060616     c                   z-add     blpmgs        sie2dri
037400060616     c                   movel     blpaas        sie2dri
037500060616     c                   call      'TISIE2R'
037600060616     c                   parm                    TIsie2ds
037700060616     c                   eval      wcalltisie2='S'
037800060616     c
037900060616     c* chain su file dpcdp01l per prendere nazione depot
038000060616     c     kcdp          chain     dpcdp01l
038100060616     c                   if        %found(dpcdp01l)
038200060616     c                   movel     cdpiso2       §DESIA
038300060616     c                   endif
038400061010     C                   eval      §DEAWB=§b4iat+§b4ipy
038500060616     c* §DEISA solo per DP4
038600060616     c                   movel     §15cod        §DEISA
038700060616     c
038800060616     c                   eval      %subst(§deeur:1:1) = §b4ibi
038900060616     c* richiamo pgm per avere il cap da stampare come vuole dpd
039000060616     c                   clear                   outcapdpd
039100060616     c                   call      'XCAPDPD'
039200060616     c                   parm                    blpcad
039300060616     c                   parm                    outcapdpd         7
039400061013     c                   parm      blpnzd        inpnzd            3
039500060616     c
039600060616     c                   eval      %subst(§deeur:2:7) = %subst(outcapdpd:1:7)
039700060616     c* Se il numero parcel è lungo 11, devo portarlo a 14
039800060616     c                   if        %subst(§b4ipn:12:1)=' '
039900060616     c                   eval      %subst(§deeur:9:1) = '0'
040000060616     c                   eval      %subst(§deeur:10:3)=%subst(§b4ipn:1:3)
040100060616     c                   eval      %subst(§deeur:13:2)='99'
040200060616     c                   eval      %subst(§deeur:15:8)=%subst(§b4ipn:4:8)
040300060616     c                   else
040400060616     c                   eval      %subst(§deeur:9:14)=§b4ipn
040500060616     c                   endif
040600060616     c
040700060616     c                   eval      %subst(§deeur:23:3)=§b4isc
040800060616     c                   eval      %subst(§deeur:26:3)=%editc(§15cie:'X')
040900060616     c* §DESIP solo per DP4
041000060616     c* ci sono i chk digits
041100060616     c                   move      §b4icd        §DESIP
041200060616     c                   clear                   trul28ds4
041300060616     c                   eval      i284cod=%subst(§DEEUR:2:27)
041400060616     c                   call      'TRUL28R5'
041500060616     c                   parm                    trul28ds4
041600060616     c                   movel     o284ckd       §DESIP
041700060616     c
041800060616     c                   eval      %subst(§DEXX1:1:7)=sie2verd
041900060616     c                   eval      %subst(§DEXX1:8:4)=§b4ida
042000060616     c                   eval      %subst(§DEXX1:12:4)=§b4ids
042100060616     c
042200060616     C                   MOVEL     DPDTEL        §DEXX2
042300060616     c                   movel     §b4isc        ksrvc
042400060616     c     kcsc          chain     dpcsc01l
042500060616     c                   if        %found(dpcsc01l)
042600060620     c                   evalr     W016A = %trim(cscsrvd)
042700060620     c                   move      w016A         §DEXX2
042800060616     c                   endif
042900060616     C                   movel(P)  dpdfax        §LDCDP
043000060616     c
043100000203    2C                   ENDIF
043200000203    1C                   ENDIF
043300971219     C                   ENDSR
043400990204     C**
043500971219     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
043600971219     C     ETIEST        BEGSR
043700971219     C                   MOVEL     '3A'          COD
043800051020     C                   MOVEL(P)  blpCBO        KEY
043900971219     C     KTAB2         CHAIN     TABEL                              31
044000971219     C  N31TBLFLG        IFNE      ' '
044100971219     C                   SETON                                        31
044200971219     C                   END
044300971219     C**
044400971219     C                   MOVEL     TBLUNI        DS3A
044500971219     C                   MOVEL     §3ATB1        FLGTB1            1
044600971219     C**
044700971219    3C                   SELECT
044800971219     C     FLGTB1        WHENEQ    'A'
044900971219     C     §3AFCA        ANDEQ     0
045000971219     C                   MOVEL     'A'           §DEFCA
045100971219     C     FLGTB1        WHENEQ    'A'
045200971219     C     §3AFCA        ANDNE     0
045300971219     C                   MOVEL     'X'           §DEFCA
045400971219     C     §3AFCA        WHENNE    0
045500971219     C                   MOVEL     'S'           §DEFCA
045600971219    3C                   ENDSL
045700971219     C                   ENDSR
045800050418      *---------------------------------------------------------------*
045900050418      *  Verifica se spedizione "DI VALORE"                           *
046000050418      *---------------------------------------------------------------*
046100050418     c     CtrSpeVal     BEGSR
046200050418      *
046300050418if  1c                   if        NOT %open(FIAR501L)
046400050418     c                   open      FIAR501L
046500050418e   1c                   endif
046600050418      *
046700050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
046800051019if  1c                   if            blpaas = AR5aas
046900051019     c                             and blplnp = AR5lnp
047000051019     c                             and blpnrs = AR5nrs
047100051019     c                             and blpnsp = AR5nsp
047200050418     c                   leavesr
047300050418e   1c                   endif
047400050418      *
047500050418      * Reperisce i dati da FIAR5
047600050418     c                   clear                   dAR5gen
047700050418     c     K05AR501      setgt     FIAR5000
047800050418do  1c                   dou            %eof(FIAR501L)
047900050418     c                             or   AR5atb =  *blanks
048000050418     c     K05AR501      readpe    FIAR5000
048100050418e   1c                   enddo
048200050418if  1c                   if             NOT %eof(FIAR501L)
048300050418     c                             and  AR5atb =  *blanks
048400050418     c                   movel     AR5uni        dAR5gen
048500050418e   1c                   endif
048600050418      *
048700050418     c                   ENDSR
048800051020     c
048900051020      *---------------------------------------------------------------*
049000051020     C* CARICO p.o. gestiti
049100051020      *---------------------------------------------------------------*
049200051020     c     carpog        begsr
049300051020     C*
049400051020     c                   clear                   trul06ds
049500051020     c                   if        dutlpo = '2'
049600051020     c                   movel     dutpou        d06key
049700051020     c                   eval      d06cod = '£6'
049800051020     c                   else
049900051020     c                   movel     simfel        d06key
050000051020     c                   eval      d06cod = '£1'
050100051020     c                   endif
050200051020     c                   movel     trul06ds      kpjbu
050300051020     c                   call      'TRUL06R'
050400051020     c                   parm                    kpjba
050500051020     c                   movel     kpjbu         trul06ds
050600051020     c                   movel     lin           pog
050700051020     c                   endsr
050800050418      *
050900970715     OFNLV22P   T    LRNLR
