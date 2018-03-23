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
004800030612      * DS per pgm di STAMPA BOLLE
004900030613     d FNLSB5DS      e ds                  inz
005000030613      *
005100060519     D*DSBL4F        E DS
005200060519     D DSBL4I        E DS
005300970715     D DSBLD         E DS
005400971219     D DSBLDE        E DS
005500000201     D OG143         E DS
005600040518     D OG150         E DS
005700971219     D DS15          E DS
005800971219     D DS3A          E DS
005900940216     D KPJBA         E DS
006000031028     d dsbl4a        e ds
006100050418     d dAR5gen       e ds                  inz
006200060519     d w005a           s              5
006300060620     d w016a           s             16
006400060616     d wcalltisie2     s              1
006500060616     d ksrvc           s                   like(cscsrvc)
006600911206      ****************************************************************
006700911206      *  RIEPILOGO INDICATORI
006800911210      ****************************************************************
006900911206      *****************************************************************
007000000000     C     *ENTRY        PLIST
007100000000     C                   PARM                    KPJBA
007200051019     C                   MOVEL     KPJBU         fnlss4ds
007300051020     c*
007400051020     c     *dtaara       define    §azute        azuteds
007500051020     c     *dtaara       define    §datiute      ddatiute
007600051020     c                   in(E)     *dtaara
007700051020     c                   If        %error  or RSUT = *blanks
007800051020     c                   CLEAR                   tibs34ds
007900051020     c                   CALL      'TIBS34R'
008000051020     c                   PARM                    tibs34ds
008100051020     c                   in        *dtaara
008200051020     c                   EndIf
008300911211      *---------------------------------------------------------------*
008400911211      * CHIAVE X POSIZIONAMENTO SU FLARB35C
008500911211      *
008600060220     C     Kar42         KLIST
008700051019     C                   KFLD                    blpAAS
008800051019     C                   KFLD                    blpLNP
008900051019     C                   KFLD                    blpNRS
009000051019     C                   KFLD                    blpNSP
009100970203     C                   KFLD                    WTRC
009200051020     C     Kblp          KLIST
009300051020     C                   KFLD                    is4saa
009400051020     C                   KFLD                    is4sln
009500051020     C                   KFLD                    is4ssr
009600051020     C                   KFLD                    is4ssp
009700051020     C* Chiave di accesso per il file tabelle
009800051020     C     KTAB1         KLIST
009900051020     C                   KFLD                    CODUT
010000051020     C                   KFLD                    KCOD
010100051020     C                   KFLD                    KKEY
010200971219     C     KTAB2         KLIST
010300971219     C                   KFLD                    CODUT             1 0
010400971219     C                   KFLD                    COD               2
010500971219     C                   KFLD                    KEY               8
010600971219     C                   Z-ADD     1             CODUT
010700060616     C     KCDP          KLIST
010800060616     C                   KFLD                    sie2ver
010900060616     C                   KFLD                    §b4ida
011000060616     C     KCSC          KLIST
011100060616     C                   KFLD                    sie2ver
011200060616     C                   KFLD                    ksrvc
011300050418      * FIAR501L
011400050418     c     k05AR501      klist
011500051019     c                   kfld                    blpaas
011600051019     c                   kfld                    blplnp
011700051019     c                   kfld                    blpnrs
011800051019     c                   kfld                    blpnsp
011900051019     c                   kfld                    aR5trd
012000050418     c                   eval      AR5trd = 'GEN'
012100970203     C* DEFINIZIONE CAMPI
012200020207     C     *LIKE         DEFINE    §OGNTW        LNANTW
012300020207     C     *LIKE         DEFINE    §OGNTW        LNPNTW
012400060220     C     *LIKE         DEFINE    ar4TRC        WTRC
012500911206      *---------------------------------------------------------------*
012600040519
012700051019     c* Eseguo sql per estrarre da fnblp solo i dati che mi servono
012800051020     c                   if        is4drt > *zeros
012900051019     c                   exsr      sr_SqlBlp
013000051019     c* Lettura ed elaborazione di fnblp
013100051020     c                   if        is4lnp = *zeros
013200051020     c                   exsr      carpog
013300051020     c                   endif
013400051019     c                   exsr      sr_LeggiBlp
013500051020     c                   else
013600051020     c* se richiesta una singola spedizione chaino e stampo
013700051020     c                   open      fnblp01l
013800051020     c     kblp          chain     fnblp01l
013900051020     c                   if        %found(fnblp01l)
014000051021     c                   exsr      S_OrgLna
014100060220     c                   ExSr      Legar4
014200051020     c                   ExSr      StaNsc
014300051020     c                   endif
014400051020     c                   close     fnblp01l
014500051020     c                   endif
014600911206      *
014700060616     c                   if        wcalltisie2='S'
014800060616     c                   clear                   tisie2ds
014900060616     c                   eval      sie2tla='C'
015000060616     c                   call      'TISIE2R'
015100060616     c                   parm                    tisie2ds
015200060616     c                   endif
015300040519     c*
015400911206     C                   SETON                                        LR
015500051019     c**********************************************************************
015600051019     c     Sr_Sqlblp     begsr
015700051019     c                   eval      StringaSql =
015800051019     c                             'Select * '                   +
015900051019     c                             'from fnblp00f WHERE blpdrt = '       +
016000051019     c                              %editc(is4drt:'4') +
016100051019     c                             ' and ' +
016200051019     c                             ' ( ' +
016300051019     c                              %editc(is4ccm:'4') +
016400051019     c                              ' in blpksc or '   +
016500051019     c                              %editc(is4ccm:'4') +
016600051019     c                              ' in blpccm'  +
016700051020     c                             ' ) ' +
016800051019     c                             ' ORDER BY blpaas, blplnp, blpnrs, blpnsp ' +
016900051019     c                             'for fetch only'
017000051019     C/EXEC SQL
017100051019     C+ prepare S1 from :StringaSql
017200051019     C/END-EXEC
017300051019     C/EXEC SQL
017400051019     C+ declare C1 cursor for S1
017500051019     C/END-EXEC
017600051019     c                   endsr
017700051019     c**********************************************************************
017800051019     c     Sr_LeggiBlp   begsr
017900051019     c*
018000051019     c* apertura cursore
018100051019     c*
018200051019     C/EXEC SQL
018300051019     C+ Open C1
018400051019     C/END-EXEC
018500051019     c*
018600051019     c* lettura cursore
018700051019     c*
018800051019     C/EXEC SQL
018900051019     C+ Fetch C1 into :fnblpxxds
019000051019     C/END-EXEC
019100051019     c                   Dow       Sqlcod = 0
019200051019     c* ignoro spedizione di recupero
019300051019    6C     BLPCBO        IFNE      $CBLPR
019400051019     C                   MOVE      '3A'          KCOD
019500051019     C                   MOVE      *BLANKS       KKEY
019600051019     C                   MOVEL     BLPCBO        KKEY
019700051019     C     KTAB1         CHAIN     TABEL00F                           99
019800051019     C  N99              MOVEL     TBLUNI        DS3A
019900051019     C  N99              MOVE      BLPCBO        $CBLPR
020000051019     C   99              CLEAR                   DS3A
020100051019     c                   endif
020200051019     c                   if        §3arbl <> 'R'
020300051021     c                   exsr      s_OrgLna
020400051020     c* scarto il record se lna non dpd o se lna letta <> da quella scelta
020500051020     c                   if        lnantw <> 'DPD' or
020600051020     c                             (is4lna > *zeros and is4lna <> blplna)
020700051021     c                   goto      leggi
020800051020     c                   endif
020900051020     c* scarto il record se richiesto un cod.tariffa specifico ed è diverso
021000051020     c* da quello letto
021100051020     c                   if        is4ctr <> 999 and is4ctr <> blpctr
021200051021     c                   goto      leggi
021300051020     c                   endif
021400051020     c* scarto il record se lnp non gestibile dall'utente o se immessa una
021500051020     c* lnp specifica ed è diversa da quella letta
021600051020     c                   if        is4lnp > *zeros and is4lnp <> blplnp
021700051021     c                   goto      leggi
021800051020     c                   endif
021900051020     c                   if        is4lnp = *zeros
022000051020     c     blplnp        lookup    pog                                    30
022100051020     c                   if        *in30 = *off
022200051021     c                   goto      leggi
022300051020     c                   endif
022400051020     c                   endif
022500060220     c                   ExSr      Legar4
022600051019     c                   ExSr      StaNsc
022700051019     c                   endif
022800051021     c*
022900051021     c     leggi         tag
023000051019     C/EXEC SQL
023100051019     C+ Fetch C1 into :fnblpxxds
023200051019     C/END-EXEC
023300051019
023400051019     c                   EndDo
023500051019     c*
023600051019
023700051019     C/EXEC SQL
023800051019     C+ close C1
023900051019     C/END-EXEC
024000051019
024100051019     c                   endsr
024200051021     C** REPERISCO DATI DI AZORG DELLA LINEA DI ARRIVO ---------------------
024300051021     c     S_OrgLna      begsr
024400051021     C     blpLNA        IFNE      SAVLNA
024500051021     C     blpLNA        CHAIN     AZORG01L                           31
024600051021     C                   MOVE      ORGDE3        OG143
024700051021     C                   MOVEl     ORGDf0        OG150
024800051021     C                   MOVE      §OGNTW        LNANTW
024900051021     c                   clear                   dpdtel           15
025000051021     c                   clear                   dpdfax           15
025100051021     c                   movel     orgtel        dpdtel
025200051021     c                   movel     orgfax        dpdfax
025300051021     C                   Z-ADD     blplna        SAVLNA            3 0
025400051021     C                   END
025500051021     c                   endsr
025600970715     C**
025700970715     C* IMPOSTO CAMPI PER STAMPA IN IMMEDIATO ------------------------*
025800970715     C     IMPO          BEGSR
025900050418      *
026000050418      * Verifica se spedizione "di valore"
026100050418     c                   exsr      CtrSpeVal
026200970715     C*
026300970715     C                   Z-ADD     0             §LDNC1
026400051019     C                   MOVEL     blpFLS        §LDLNP
026500051019     C     blpFLS        CHAIN     AZORG                              30
026600970715     C  N30              MOVEL     ORGDES        §LDDLP
026700970715     C   30              MOVEL     *BLANKS       §LDDLP
026800051019     C                   MOVEL     blpNRS        §LDNRS
026900051019     C                   MOVEL     blpLNA        §LDLNA
027000051019     C     blpLNA        CHAIN     AZORG                              30
027100970715     C  N30ORGDE5        IFNE      *BLANKS
027200970715     C                   MOVEL     ORGDE5        §LDDLA
027300970715     C                   ELSE
027400970715     C                   MOVEL     ORGDES        §LDDLA
027500970715     C                   END
027600970715     C   30              MOVEL     *BLANKS       §LDDLA
027700051019     C                   MOVEL     blpTFA        §LDTNA
027800051019     C                   MOVEL     blpTSP        §LDTSP
027900051019     C                   MOVEL     blpZNC        §LDZSC
028000051019     C                   MOVE      blpNCL        §LDNC2
028100051019     C                   MOVEL     blpRSM        §LDRSM
028200051019     C                   MOVEL     blpPRM        §LDPRM
028300051019     C                   MOVEL     blpRSD        §LDRSD
028400051019     C                   MOVEL     blpCAD        §LDCAD
028500051019     C                   MOVEL     blpLOD        §LDLOD
028600051019     C                   MOVEL     blpIND        §LDIND
028700051019     C                   MOVEL     blpPRD        §LDPRD
028800970715     C                   MOVEL     *ZEROS        §LDNCP
028900970715     C                   MOVEL     *ZEROS        §LDNCU
029000050418if  1c                   if        §AR5bva = 'V'
029100050418     c                   movel     'X'           §LDrst
029200050418x   1c                   else
029300970715     C                   MOVEL     'R'           §LDRST
029400050418e   1c                   endif
029500051019     C                   Z-ADD     blpPKF        §LDPKG
029600051019     C                   Z-ADD     blpVLF        §LDVOL
029700051019     C                   MOVEL     blpFVF        §LDFVR
029800051019     C                   MOVEL     blpAAS        §LDAAS
029900051019     C                   MOVEL     blpMGS        §LDMGS
030000971219     C*
030100040519     c* Se sto stampando i segnacolli in arrivo in base al file di work
030200040519     c* devo stampare solo l'etichetta estera
030300040519     c                   Eval      §Detst = 'EST'
030400040519     c                   exsr      etiest
030500971219     C*
030600970715     C                   ENDSR
030700970715     C**
030800030414      **
030900030414      *--- STAMPA SEGNACOLLO -----------------------------------------*
031000030414     c     StaNsc        BegSr
031100030414
031200030414     c                   ExSr      Impo
031300030414
031400030414     c                   Add       1             §LDNC1
031500051019     c                   Movel     blpncd        §LDNCD
031600030414
031700030414
031800051020     c                   CALL      i90pss
031900030414     c                   PARM                    DSBLD
032000030414     c                   PARM                    DSBLDE
032100030414
032200030414     c                   EndSr
032300030414
032400971219     C*--- REPERISCO ESTENZIONE BOLLA TIPO RECORD 'E' ----------------*
032500060220     C     legar4        BEGSR
032600000203     C                   CLEAR                   DSBLDE
032700000203     C                   CLEAR                   DSBLD
032800971219     C*
032900051019     C                   MOVEL     blprma        §LDCDP
033000971219     C*
033100971219     C*
033200051019     C     blpLNP        CHAIN     AZORG01L                           31
033300020207     C                   MOVE      ORGDE3        OG143
033400020207     C                   MOVE      §OGNTW        LNPNTW
033500060616     C*
033600060616     C* CHAIN SULLE NAZIONI
033700060616     C                   MOVEL     '15'          COD
033800060616     C                   MOVEL(P)  blpNZD        KEY
033900060616     C     KTAB2         CHAIN     TABEL                              31
034000060616     C  N31TBLFLG        IFNE      ' '
034100060616     C                   SETON                                        31
034200060616     C                   END
034300060616     C**
034400060616     C  N31              MOVEL     TBLUNI        DS15
034500060616     C   31              CLEAR                   DS15
034600060616     C                   MOVEL     §15COD        §DEISA
034700000207     C*
034800000203      **
034900060220      **  CHAIN Fiar4 PER EXPORT DPD
035000020207    1C     LNANTW        IFEQ      'DPD'
035100060519     C                   MOVEL     'I'           WTRC
035200060220     C     Kar42         CHAIN     Fiar401L                           39
035300060519     C  N39              MOVEL     ar4NOT        DSBL4I
035400060519     C   39              CLEAR                   DSBL4I
035500060519    2C     §B4ipn        IFNE      *BLANKS
035600060616     C***                MOVEL     §B4ipn        DPDPAR
035700060616     C***                MOVEL     DPD1          DPDS1
035800060616     C***                MOVEL     DPD2          DPDS2
035900060616     C***                MOVEL     §b4icd        DPDS3
036000060616     C***                MOVEL(P)  DPDSTP        §LDCDP
036100011011     C* DEVO SEMPRE RISTAMPARE ANCHE IL CHI SONO
036200040518     c*
036300060616     c***  §ogsea        ifne      'S'
036400060616     C***                MOVEL     'DP2'         §DETSE
036500060616     c***                else
036600060616     c***                movel     'DP3'         §detse
036700060616     c***                endif
036800060616     c
036900051019     C     blpPKF        IFLE      3
037000000203     C                   MOVEL     'X'           §DESSE
037100000203     C                   ENDIF
037200060616     c                   movel     'DP4'         §detse
037300060616     c* reperisco versione cappario DPD
037400060616     c                   clear                   tisie2ds
037500060616     c                   z-add     blpmgs        sie2dri
037600060616     c                   movel     blpaas        sie2dri
037700060616     c                   call      'TISIE2R'
037800060616     c                   parm                    TIsie2ds
037900060616     c                   eval      wcalltisie2='S'
038000060616     c
038100060616     c* chain su file dpcdp01l per prendere nazione depot
038200060616     c     kcdp          chain     dpcdp01l
038300060616     c                   if        %found(dpcdp01l)
038400060616     c                   movel     cdpiso2       §DESIA
038500060616     c                   endif
038600061010     C                   eval      §DEAWB=§b4iat+§b4ipy
038700060616     c* §DEISA solo per DP4
038800060616     c                   movel     §15cod        §DEISA
038900060616     c
039000060616     c                   eval      %subst(§deeur:1:1) = §b4ibi
039100060616     c* richiamo pgm per avere il cap da stampare come vuole dpd
039200060616     c                   clear                   outcapdpd
039300060616     c                   call      'XCAPDPD'
039400060616     c                   parm                    blpcad
039500060616     c                   parm                    outcapdpd         7
039600061013     c                   parm      blpnzd        inpnzd            3
039700060616     c
039800060616     c                   eval      %subst(§deeur:2:7) = %subst(outcapdpd:1:7)
039900060616     c* Se il numero parcel è lungo 11, devo portarlo a 14
040000060616     c                   if        %subst(§b4ipn:12:1)=' '
040100060616     c                   eval      %subst(§deeur:9:1) = '0'
040200060616     c                   eval      %subst(§deeur:10:3)=%subst(§b4ipn:1:3)
040300060616     c                   eval      %subst(§deeur:13:2)='99'
040400060616     c                   eval      %subst(§deeur:15:8)=%subst(§b4ipn:4:8)
040500060616     c                   else
040600060616     c                   eval      %subst(§deeur:9:14)=§b4ipn
040700060616     c                   endif
040800060616     c
040900060616     c                   eval      %subst(§deeur:23:3)=§b4isc
041000060616     c                   eval      %subst(§deeur:26:3)=%editc(§15cie:'X')
041100060616     c* §DESIP solo per DP4
041200060616     c* ci sono i chk digits
041300060616     c                   move      §b4icd        §DESIP
041400060616     c                   clear                   trul28ds4
041500060616     c                   eval      i284cod=%subst(§DEEUR:2:27)
041600060616     c                   call      'TRUL28R5'
041700060616     c                   parm                    trul28ds4
041800060616     c                   movel     o284ckd       §DESIP
041900060616     c
042000060616     c                   eval      %subst(§DEXX1:1:7)=sie2verd
042100060616     c                   eval      %subst(§DEXX1:8:4)=§b4ida
042200060616     c                   eval      %subst(§DEXX1:12:4)=§b4ids
042300060616     c
042400060616     C                   MOVEL     DPDTEL        §DEXX2
042500060616     c                   movel     §b4isc        ksrvc
042600060616     c     kcsc          chain     dpcsc01l
042700060616     c                   if        %found(dpcsc01l)
042800060620     c                   evalr     W016A = %trim(cscsrvd)
042900060620     c                   move      w016A         §DEXX2
043000060616     c                   endif
043100060616     C                   movel(P)  dpdfax        §LDCDP
043200060616     c
043300000203    2C                   ENDIF
043400000203    1C                   ENDIF
043500971219     C                   ENDSR
043600990204     C**
043700971219     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
043800971219     C     ETIEST        BEGSR
043900971219     C                   MOVEL     '3A'          COD
044000051020     C                   MOVEL(P)  blpCBO        KEY
044100971219     C     KTAB2         CHAIN     TABEL                              31
044200971219     C  N31TBLFLG        IFNE      ' '
044300971219     C                   SETON                                        31
044400971219     C                   END
044500971219     C**
044600971219     C                   MOVEL     TBLUNI        DS3A
044700971219     C                   MOVEL     §3ATB1        FLGTB1            1
044800971219     C**
044900971219    3C                   SELECT
045000971219     C     FLGTB1        WHENEQ    'A'
045100971219     C     §3AFCA        ANDEQ     0
045200971219     C                   MOVEL     'A'           §DEFCA
045300971219     C     FLGTB1        WHENEQ    'A'
045400971219     C     §3AFCA        ANDNE     0
045500971219     C                   MOVEL     'X'           §DEFCA
045600971219     C     §3AFCA        WHENNE    0
045700971219     C                   MOVEL     'S'           §DEFCA
045800971219    3C                   ENDSL
045900971219     C                   ENDSR
046000050418      *---------------------------------------------------------------*
046100050418      *  Verifica se spedizione "DI VALORE"                           *
046200050418      *---------------------------------------------------------------*
046300050418     c     CtrSpeVal     BEGSR
046400050418      *
046500050418if  1c                   if        NOT %open(FIAR501L)
046600050418     c                   open      FIAR501L
046700050418e   1c                   endif
046800050418      *
046900050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
047000051019if  1c                   if            blpaas = AR5aas
047100051019     c                             and blplnp = AR5lnp
047200051019     c                             and blpnrs = AR5nrs
047300051019     c                             and blpnsp = AR5nsp
047400050418     c                   leavesr
047500050418e   1c                   endif
047600050418      *
047700050418      * Reperisce i dati da FIAR5
047800050418     c                   clear                   dAR5gen
047900050418     c     K05AR501      setgt     FIAR5000
048000050418do  1c                   dou            %eof(FIAR501L)
048100050418     c                             or   AR5atb =  *blanks
048200050418     c     K05AR501      readpe    FIAR5000
048300050418e   1c                   enddo
048400050418if  1c                   if             NOT %eof(FIAR501L)
048500050418     c                             and  AR5atb =  *blanks
048600050418     c                   movel     AR5uni        dAR5gen
048700050418e   1c                   endif
048800050418      *
048900050418     c                   ENDSR
049000051020     c
049100051020      *---------------------------------------------------------------*
049200051020     C* CARICO p.o. gestiti
049300051020      *---------------------------------------------------------------*
049400051020     c     carpog        begsr
049500051020     C*
049600051020     c                   clear                   trul06ds
049700051020     c                   if        dutlpo = '2'
049800051020     c                   movel     dutpou        d06key
049900051020     c                   eval      d06cod = '£6'
050000051020     c                   else
050100051020     c                   movel     simfel        d06key
050200051020     c                   eval      d06cod = '£1'
050300051020     c                   endif
050400051020     c                   movel     trul06ds      kpjbu
050500051020     c                   call      'TRUL06R'
050600051020     c                   parm                    kpjba
050700051020     c                   movel     kpjbu         trul06ds
050800051020     c                   movel     lin           pog
050900051020     c                   endsr
051000050418      *
051100970715     OFNLV22P   T    LRNLR
