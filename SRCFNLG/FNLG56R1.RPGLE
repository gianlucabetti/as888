000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000300900418     H*                                                              *
000400121002     H*   - GIACENZE CLIENTE -
000500900423     H*                                                              *
000600900418     H*--------------------------------------------------------------*
000601120928     Ftigcp53l  iF   E           k disk
000700121003     Ftitas30c  iF   E           k disk    USROPN
000701121005     F                                     ignore(titasp00)
000702121002     FCNACO00F  IF   E           K DISK
000800120928     Ftabel00f  IF   E           K DISK
000801121001     Fwtigcp00f uf a E           k disk
000802121001     FQSYSPRT   O    F  132        PRINTER
000803120928      *-----------------------------------------------------
002700900418     D KPJBA         E DS
002701121001     D Dspr          E DS
002702121001     D Ds14          E DS
002703120928     D fnlg56ds      e ds
002704121001     d coman           s             99
002705121001     d luncmd          s             15  5
002706121001     d file            s             10
002707121001     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002708121001     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002709121001     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002710121001     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
002711121001     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
002712121001     D cmd6            S              1    DIM(99) CTDATA PERRCD(99)
002713121003     D Cmd7            S             49    dim(1) ctdata perrcd(1)
002714121003     D Cmd8            S             49    dim(1) ctdata perrcd(1)
002715121003     D Cmd             S             49
002716121001     d dataiso         s               d   datfmt(*iso)
002717121001     d dataeur         s               d   datfmt(*eur)
002718121001     d dataape         s               d   datfmt(*iso)
002719121001     d datachiu        s               d   datfmt(*iso)
002720121001     d dexreg          s             25
002721121003     d exreg           s              3  0
002722121001     d giorni          s              5  0
002723121001     d giornitot       s              6  0
002724121001     d diffe           s              5  0
002725121001     d giacres         s              5  0
002726121001     d giaccon         s              5  0
002727121001     d giactot         s              5  0
002728121003     d giacrest        s              6  0
002729121003     d giaccont        s              6  0
002730121003     d giactott        s              6  0
002731121001     d media           s              5  0
002732121001     d data8           s              8  0
002733121001     d datas1          s              8  0
002734121001     d datas2          s              8  0
002735121002     D TCUDS           DS
002736121002     D  F34                    3      4
002737121002     D  F56                    5      6
002738121002     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002739121002     D  TCU                  398    697
002740121002     D                                     DIM(50)
002741121002     D  KCU                  698    847P 0
002742121002     D                                     DIM(50)
002743121002     D                                     PACKEVEN
002744121001      *______________________________________________________________________
002745120928      *
002746121001      * creo nome file per dati utente in libreria di wrk
002747121001     c*m                 eval      file = 'AU' + knmus
002748121001      *clear file
002749121001     c                   movea     cmd1          coman
002750121001     c                   exsr      comandi
002751121002     c                   movea     cmd2          coman
002752121002     c                   exsr      comandi
002753121001      *
002754121001      *genera file
002755120928     c                   exsr      elabora
002756121001      *
002757121001      *genera report per regione
002758121001     c                   exsr      elabora2
002759121001      *verifica se esiste file in libreria di wrk per utente
002760121001     c*m                 movea     cmd4          coman
002761121001     c*m                 eval      %subst(coman:22:10) = file
002762121001     c*m                 exsr      comandi2
002763121001      *se trova il file esegue copy file altrimenti lo crea
002764121001     c*m                 if        *in34
002765121001     c*m                 movea     cmd5          coman
002766121001     c*m                 eval      %subst(coman:78:10) = file
002767121001     c*m                 exsr      comandi2
002768121001     c*m                 else
002769121001     c*m                 movea     cmd6          coman
002770121001     c*m                 eval      %subst(coman:41:10) = file
002771121001     c*m                 exsr      comandi2
002772121001      *elimina file in qtemp x la filiale
002773121009     c                   movea     cmd3          coman
002774121009     c                   exsr      comandi
002775121001     c                   seton                                        lr
002776120928      *
015302120928     C*-----------------------------------------------------*
015303120928     c     elabora       begsr
015304120928     C*-----------------------------------------------------*
015305120928     c     d56ksc        setll     tigcp53l
015306120928     c                   do        *hival
015307120928     c     d56ksc        reade     tigcp53l
015308120928     c                   if        %eof(tigcp53l)
015309120928     c                   leave
015310120928     c                   endif
015311120928     c                   if        gcpfrg > 0    or
015312121002     c                             gcpfas <= 40  or
015313120928     c                             gcpdcg = 0    or
015314120928     c                             gcpdcg < d56datad or
015315121009     c                             gcpdcg > d56dataa
015317120928     c                   iter
015318120928     c                   endif
015319120928     c     kbol          chain     titas30c
015320120928     c                   if        %found(titas30c)
015321120928     c                   movel     tasprd        wprov
015322121001     c                   movel     'PR'          tblcod
015323120928     c                   movel     tasprd        tblkey
015324120928     c     ktab          chain     tabel00f
015325120928     c                   if        %found(tabel00f)
015326120928     c                   movel     tbluni        dspr
015327120928     c                   move      §prcre        wreg
015330121001     c                   write     wtigcp00
015331120928     c                   endif
015332120928     c                   endif
015333120928     c                   enddo
015334120928     c                   endsr
015335121001     C*-----------------------------------------------------*
015336121001     c     elabora2      begsr
015337121001     C*-----------------------------------------------------*
015338121001     c     *loval        setll     wtigcp00f
015339121001     c                   do        *hival
015340121001     c                   read      wtigcp00f
015341121001     c                   if        %eof(wtigcp00f)
015342121001     c                   leave
015343121001     c                   endif
015344121001      * imposta prima regione e stampa testata
015345121001     c                   if        exreg = *zero
015346121001     c                   move      wreg          exreg
015347121001     c                   except    testata
015348121001     c                   endif
015349121001      * calcoli finali media giorni sosta per regione e stampa dettaglio
015351121001      *
015352121001     c                   if        exreg <> wreg
015353121001      *decodifica regione
015354121001     c                   movel     '14'          tblcod
015355121001     c                   movel     exreg         tblkey
015356121001     c     ktab          chain     tabel00f
015357121001     c                   if        %found(tabel00f)
015358121001     c                   movel     tbluni        ds14
015359121001     c                   movel     §14des        dexreg
015360121001     c                   endif
015361121001      *
015362121001     c     giaccon       add       giacres       giactot
015363121001     c     giornitot     div       giactot       media             5 0
015364121001     c                   except    dettaglio
015365121001     c                   move      wreg          exreg
015366121003     c                   add       giaccon       giaccont
015367121003     c                   add       giacres       giacrest
015368121003     c                   add       giactot       giactott
015369121001     c                   clear                   giaccon
015370121001     c                   clear                   giacres
015371121001     c                   clear                   giactot
015372121001     c                   clear                   media
015373121001     c                   clear                   giorni
015374121001     c                   clear                   giornitot
015375121001     c                   endif
015376121001      * totalizza per regione
015377121001      *
015378121001     c                   clear                   data8
015379121001     c                   movel     gcpagc        data8
015380121001     c                   move      gcpmgc        data8
015381121001     c                   move      data8         dataape
015382121001     c                   move      gcpdcg        datachiu
015383121001     c     datachiu      subdur    dataape       giorni:*d
015384121001     c                   add       giorni        giornitot
015385121009     c                   if        gcpdis  <> 'R'
015386121001     c                   add       1             giaccon           5 0
015387121001     c                   else
015388121001     c                   add       1             giacres           5 0
015389121001     c                   endif
015390121001     c                   enddo
015391121003     c                   except    totali
015392121001     c                   endsr
015393121001     c*-------------------------------------------------
015394121001     c     comandi       begsr
015395121001     c*-------------------------------------------------
015396121001     C                   close     wtigcp00f
015397121001      *
015398121001     C                   z-add     99            luncmd
015399121001     C                   CALL      'QCMDEXC'
015400121001     C                   parm                    coman
015401121001     C                   parm                    luncmd
015402121001      *
015403121001     C                   OPEN      wtigcp00f
015404121001     c                   endsr
015405121001     c*-------------------------------------------------
015406121001     c     comandi2      begsr
015407121001     c*-------------------------------------------------
015408121001      *
015409121001     C                   z-add     99            luncmd
015410121001     C                   CALL      'QCMDEXC'                            34
015411121001     C                   parm                    coman
015412121001     C                   parm                    luncmd
015413121001      *
015414121001     c                   endsr
015415940926     C*-----------------------------------------------------*
015500940926     C*  Operazioni iniziali
015600940926     C*------------------------------------------------------*
015700940926     C     *INZSR        BEGSR
015800940926     C*
015900940926     C     *ENTRY        PLIST
015901121003     C                   PARM                    kpjba
016000121003     C                   movel     kpjbu         fnlg56ds
016001121002     C                   Z-ADD     1             CODUT
016002121002     C                   CALL      'X§PARUT'
016003121002     C                   PARM                    UTEDSE
016005121002     C                   MOVEL     RAGUT         RSUT             20
016006121002     C*--- RICERCA CAPOCONTI------------------------------------------*
016007121002     C                   DO        50            X                 2 0
016008121002     C                   MOVE      TCU(X)        TCUDS
016009121002     C     F56           IFEQ      'CG'
016010121002     C     F34           ANDEQ     '01'
016011121002     C                   Z-ADD     KCU(X)        KCI               4 0
016012121002     C                   END
016013121002     C                   END
016014120928     c*
016015120928     c     ktab          klist
016016120928     c                   kfld                    tblkut
016017120928     c                   kfld                    tblcod
016018120928     c                   kfld                    tblkey
016019120928     c                   z-add     1             tblkut
016020121002      *
016021120928     c     kbol          klist
016022120928     c                   kfld                    gcpaas
016023120928     c                   kfld                    gcplnp
016024120928     c                   kfld                    gcpnrs
016025120928     c                   kfld                    gcpnsp
016026121002      *
016027121002     C     kaco          klist
016028121002     C                   kfld                    codut
016029121002     C                   kfld                    kci
016030121002     C                   kfld                    d56ksc
016031121002      *
016032121001     c                   move      d56datad      dataiso
016033121001     c                   move      dataiso       dataeur
016034121001     c                   move      dataeur       datas1
016035121001     c                   move      d56dataa      dataiso
016036121001     c                   move      dataiso       dataeur
016037121001     c                   move      dataeur       datas2
016038121001     C                   time                    w0120            14 0
016039121001     C                   move      w0120         wdat              8 0
016040121001     C                   movel     w0120         wora              6 0
016041120928      *
016043121002     C     KACO          CHAIN     CNACO000                           40
016044121002     c                   movel     acorag        acorag30         30
016045121003     c                   exsr      comando
016046121003     C                   open      titas30c
018400940926     C                   ENDSR
018401121003      *---------------------------------------------------------------------
018402121003     C     comando       BEGSR
018403121003      *---------------------------------------------------------------------
018404121003     c                   if        %subst(knsif:7:1) = 'P'
018405121003     C                   movea     cmd7          cmd
018406121003     c                   else
018407121003     C                   movea     cmd8          cmd
018408121003     c                   endif
018409121003     C                   movel(p)  cmd           comman
018410121003     C                   call      'QCMDEXC'
018411121003     C                   parm                    comman           80
018412121003     C                   parm      49            lung             15 5
018413121003     C*
018414121003     C                   ENDSR
019601121001      *-------------------------------------------------------------------------
019700121001     OQSYSPRT   E            TESTAta     1    03
019702121002     o                       rsut                22
019703121002     o                                           80 'Statistica Giacenze -
019704121001     o                                               per Cliente e -
019705121001     o                                               Regione'
019712121002     o                                           95 'FNLG56R1'
019713121002     O                                          105 'Pag'
019714121002     o                       page          z    110
019715121003     O          e            testata     2
019716121003     o                                           08 'Cliente'
019718121003     o                       d56ksc              16
019719121002     o                       acorag30            48
019720121001     o                                           60 'Periodo'
019721121001     o                       datas1              72 '  /  /    '
019722121001     o                                           75 'al'
019723121001     o                       datas2              86 '  /  /    '
019726121002     o                       udate         y     98
019727121002     o                       wora               110 '  :  :  '
019728121001     O          e            testata     2
019729121001     o                                           10 'Regione'
019730121001     o                                           50 'Giacenze'
019731121001     o                                           60 'Rese'
019732121001     o                                           72 'Riconsegna'
019733121003     o                                           94 'gg.Medi Giacenza'
019734121001     O          e            dettaglio   2
019735121001     o                       exreg                6
019736121001     o                       dexreg              40
019737121001     o                       giactot       z     50
019738121001     o                       giacres       z     60
019739121001     o                       giaccon       z     72
019740121001     o                       media         z     84
019741121003     O          e            totali      1
019742121003     o                                           72 '__________________________'
019743121003     O          e            totali      2
019744121003     o                                           40 'T o t a l i '
019745121003     o                       giactott      2     50
019746121003     o                       giacrest      2     60
019747121003     o                       giaccont      2     72
020100121001      *--------------------------------------------------------------
020200121001**  CMD1
020300121001CLRPFM    FILE(WTIGCP00F)
020400121001**  CMD2
020500121001CRTDUPOBJ OBJ(Wtigcp00f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
020600121001**  CMD3
020700121001DLTF FILE(QTEMP/Wtigcp00F)
020800121001**  CMD4
020900121001CHKOBJ  OBJ(WRKQUERY/xxxxxxxxxx) OBJTYPE(*FILE)
021000121001**  CMD5
021100121001CRTDUPOBJ OBJ(Wtigcp00f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xx
021200121001**  CMD6
021300121001CPYF FROMFILE(Wtigcp00F) TOFILE(WRKQUERY/xxxxxxxxxx) MBROPT(*REPLACE)
021400121003**  CMD7
021500121003OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
021600121003**  CMD8
021700121003OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRU/TITAS30C)
