000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200931130      *****************************************************************
000300021213      *  Nome programma:  FNLSC1R
000400941012      *  Descrizione   :  Stampa statistica partenze: partito
000500931130      *                   Programma esecuzione (stampa)
000600931130      *  Data creazione:  OTT 1993
000700931130      *****************************************************************
000800941012      *  Il pgm puo' essere lanciato dal filtro FNLSC0R oppure tramite
000900931130      *  le funzioni giornaliere, insieme ad altre stampe (guidate dalla
001000931130      *  tabella DS3L)
001100931130      *****************************************************************
001200950210      *  ??PASSAGGIO PARAMETRI:?
001300950216      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001400950210      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001500970207      *  DS1FPG   TIPO STASTISTICA
001600950210      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001700950210      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001800950210      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001900950210      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
002000950210      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002100950210      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002200950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002300950210      *****************************************************************
002400020813     FFNFGV06L  IF   E           K DISK
002500070112     FFNBRV09L  IF   E           K DISK
002600090911     FFiSPA01L  UF A E           K DISK
002700930930     FTABEL00F  IF   E           K DISK
002800021023     FFNBLT38L  IF   E           K DISK
002900950111     FFNBLP01L  IF   E           K DISK
003000950111     F                                     RENAME(FNBLP000:FNBLP001)
003100070523     FFNBLP05L  IF   E           K DISK
003200070523     F                                     RENAME(FNBLP000:FNBLP005)
003300020823     FFNBTT38L  IF   E           K DISK
003400020823     FFNBTP11L  IF   E           K DISK
003500020823     FFNBTP15L  IF   E           K DISK
003600020823     F                                     RENAME(FNBTP000:FNBTP015)
003700990518     FFNANM04L  IF   E           K DISK
003800930924     FAZORG01L  IF   E           K DISK
003900970204     FAZCLN01L  IF   E           K DISK
004000090415     Ffista01L  uF   E           K DISK
004100021016     fTntbe01l  uf   e           k Disk
004200930927     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
004300930927     F                                     USROPN
004400931130      *--TAB.INTERNE E SCHIERE---------------------------------------*
004500950209     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
004600970306     D SK              S             66    DIM(18) CTDATA PERRCD(1)
004700941012     D SK1             S             66    DIM(7) CTDATA PERRCD(1)
004800970204     D GME             S              1    DIM(31)
004900931130      *
005000000831     D L1              S              3  0 DIM(30)
005100020418     D L6              S              3  0 DIM(30)
005200950213     D LV2             S              3  0 DIM(30)
005300000615     D LIN             S              3  0 DIM(30)
005400000615     D SVLIN           S              3  0 DIM(30)
005500950111     D E3A             S              2    DIM(60)
005600970505     D D3A             S             75    DIM(60)
005700931130      *--SCHIERE FILIALE E AREA PER CALCOLI
005800941012      * N.B.: le schiere di 6 elementi sono cosi' suddivise:
005900941012      *       .  i primi tre (1-3) elementi sono i c/servizi dell'area
006000941012      *       .  i secondi tre (4-6)sono i totali di filiale
006100941012      *       .  i terzi tre (7-9) sono i c/servizi della filiale
006200931130      *--COLLI
006300090914     D SKA             S              7  0 DIM(20)
006400090914     D AKA             S              7  0 DIM(4)
006500090914     D TKA             S              7  0 DIM(4)
006600090914     D XKA             S              7  0 DIM(4)
006700931130      *--PESO
006800090914     D SKD             S              8  1 DIM(20)
006900090914     D AKD             S              8  1 DIM(4)
007000090914     D TKD             S              8  1 DIM(4)
007100090914     D XKD             S              8  1 DIM(4)
007200090914     D SKE             S              8  1 DIM(20)
007300090914     D AKE             S              8  1 DIM(4)
007400090914     D TKE             S              8  1 DIM(4)
007500090914     D XKE             S              8  1 DIM(4)
007600090914     D SKF             S              8  1 DIM(20)
007700090914     D AKF             S              8  1 DIM(4)
007800090914     D TKF             S              8  1 DIM(4)
007900090914     D XKF             S              8  1 DIM(4)
008000931130      *--VOLUME
008100090914     D SKI             S              9  4 DIM(20)
008200090914     D AKI             S              9  4 DIM(4)
008300090914     D TKI             S              9  4 DIM(4)
008400090914     D XKI             S              9  4 DIM(4)
008500931130      *--SPEDIZIONI
008600090914     D SKL             S              7  0 DIM(20)
008700090914     D AKL             S              7  0 DIM(4)
008800090914     D TKL             S              7  0 DIM(4)
008900090914     D XKL             S              7  0 DIM(4)
009000090914     D SKM             S              7  0 DIM(20)
009100090914     D AKM             S              7  0 DIM(4)
009200090914     D SKN             S              7  0 DIM(20)
009300090914     D AKN             S              7  0 DIM(4)
009400090914     D SKO             S              7  0 DIM(20)
009500090914     D SKP             S              7  0 DIM(20)
009600020418
009700020418     d sav_t06         S              3  0 dim(30)
009800090605
009900090605     d wdtsp           s              5    dim(10)
010000020418
010100020418     d wc              s              3  0
010200021016
010300021016     d kTbeCod         s                   Like(TbeCod)
010400021016     d kTbeKe1         s                   Like(TbeKe1)
010500050829     d klin            s                   Like(ds1lnp)
010600050829     d kdre            s                   Like(spadre)
010700020418
010800931130      *--DS*---------------------------------------------------------*
010900931130      *
011000931130      * DS PER TESTARE QUANDO CAMBIA BOLLA (SI VALORIZZA LEGGENDO I SEGNACOLLI).
011100930930     D                 DS
011200950111     D  BLTAAS                 1      4  0
011300950111     D  BLTLNP                 5      7  0
011400950111     D  BLTNRS                 8      9  0
011500950111     D  BLTNSP                10     16  0
011600950111     D  DSTXT                  1     16  0
011700950111     D                 DS
011800950111     D  BTTAAS                 1      4  0
011900950111     D  BTTLNP                 5      7  0
012000950111     D  BTTNRS                 8      9  0
012100950111     D  BTTNSP                10     16  0
012200950111     D  DSTXT1                 1     16  0
012300931130      *
012400931130      * DS PER TESTARE SE BOLLETTATO IN PERIODO O IN PERIODO PRECEDENTE.
012500930930     D                 DS
012600941012     D  BLPAAS                 1      4  0
012700941012     D  BLPMGS                 5      8  0
012800941012     D  BLPDS                  1      8  0
012900040921      *
013000040921     D WLBDAT          DS                  INZ
013100040921     D  G02DAT                 1      8  0
013200040921     D  G02INV                 9     16  0
013300040921     D  G02ERR                17     17
013400040921     D  G02TGI                18     22  0
013500921222     D DS3A          E DS
013600950111     D* N.B.: SE VARIA LA LUNGHEZZA DI DS3A, MODIF.LA SCHIERA D3A
013700930927     D DS3I          E DS
013800921222     D DS3L          E DS
013900970707     D DS5P          E DS
014000950111     D DSQT1         E DS
014100950210     D DSLS01        E DS
014200970204     D  ANNODA                41     44  0
014300970204     D  MESEDA                45     46  0
014400970204     D  $GI                   47     48  0
014500920930     D CNCR80        E DS
014600920930     D KPJBA         E DS
014700920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
014800950209     D* DS PER TRUL06R - CARICAMENTO £X
014900950209     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015000950314     D  T06                    1     90  0
015100950314     D                                     DIM(30)
015200970129      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
015300970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
015400000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
015500000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
015600021016      * Ds date statistiche
015700021016     d dSdf          e ds
015800090605
015900090605     d ds5e          e ds
016000020813      *
016100020813      * DEFINIZIONE CAMPI
016200020813      *
016300020813     D SAVRIS          S                   LIKE(DS1RIS) inz
016400020813     D WCAA            S                   LIKE(ANMCAA) inz
016500021220     D WVOLUM          S                   LIKE(VOLUM)  inz
016600021220     D WPESCOL         S                   LIKE(PESCOL) inz
016700020813     D KDUC            S                   LIKE(BLPDUC) inz
016800020813     D KDBR            S                   LIKE(BLPDBR) inz
016900070523     D KDrt            S                   LIKE(BLPDrt) inz
017000020813     D $TFA            S                   LIKE(BLPTFA) inz
017100020813     D W0010           S                   LIKE(SPASFC) inz
017200020813     D W0020           S                   LIKE(SPASFC) inz
017300020813     D W0030           S                   LIKE(SPASFC) inz
017400090914     D W0040           S                   LIKE(SPASFh) inz
017500020813     D KNPG            S                   LIKE(BRVNPG) inz
017600020813     D SAVLNP          S                   LIKE(BLPLNP) inz
017700020813     D*
017800020813     D KCOD            S                   LIKE(TBLCOD) inz
017900020813     D KKEY            S                   LIKE(TBLKEY) inz
018000020813     D COMFIL          S                   LIKE(ORGFIL) inz
018100020813     D FLDTXT          S                   LIKE(DSTXT)  inz
018200090415     D ktrc            S                   LIKE(statrc) inz('P')
018300090415     D kfil            S                   LIKE(stafil) inz(000)
018400021213      *
018500021213     d Volum           s             12  4 inz
018600021213     d PesCol          s              8  1 inz
018700021213     d PesTax          s              8  0 inz
018800021213     d PesTas          s              9  1 inz
018900021213     d Wvolu           s                   like(BLPvlf) inz
019000021213     d Wpeso           s                   like(BLPpkf) inz
019100931130      *---------------------------------------------------------------*
019200931130      *    RIEPILOGO INDICATORI
019300931130      *---------------------------------------------------------------*
019400931130      * 03    - IMMESSA DATA AL
019500970129      * 05    - ON,CLIENTE DISK B
019600931130      * 04    - ON,SE BOLLA=C/SERVIZI; OFF SE BOLLA DI FILIALE.
019700931130      * 06    - ON,SE BOLLA FINO A 35 Q. OFF SE OLTRE 35 Q.
019800931130      * 07    - ON,SE BOLLA BOLLETTATA IN PERIODO PRECEDENTE.(PER CICLO SPEDIZ.)
019900931130      * 30/32 - DI COMODO
020000940328      * 36    - ON,SE NON SI VUOLE IL TABULATO STAMPATO
020100931130      *---------------------------------------------------------------*
020200000000     C     *ENTRY        PLIST
020300000000     C                   PARM                    KPJBA
020400950210     C                   MOVEL     KPJBU         DSLS01
020500950210      *
020600931130      *
020700931130      *---------------------------------------------------------------*
020800931130      * LETTURA FILE SEGNACOLLI (COLLI).
020900930924     C     KBLT30        KLIST
021000930924     C                   KFLD                    COMFIL
021100950210     C                   KFLD                    DS1ADA
021200970129     C* LETTURA BOLLE PER CALCOLO SPEDIZIONI
021300970129     C     KBLP6         KLIST
021400970129     C                   KFLD                    COMFIL
021500970129     C                   KFLD                    KDUC
021600970204     C     KBLP6A        KLIST
021700970204     C                   KFLD                    COMFIL
021800970204     C                   KFLD                    KDUC
021900970204     C                   KFLD                    KDBR
022000970129     C     KBLP6B        KLIST
022100970129     C                   KFLD                    COMFIL
022200970129     C                   KFLD                    BLPDUC
022300970204     C     KBLP6C        KLIST
022400970204     C                   KFLD                    COMFIL
022500970204     C                   KFLD                    BLPDUC
022600970204     C                   KFLD                    BLPDBR
022700070523     C* LETTURA BOLLE PER CALCOLO SPEDIZIONI
022800070523     C     KBLP5         KLIST
022900070523     C                   KFLD                    COMFIL
023000070523     C                   KFLD                    Kduc
023100070523     C     KBLP5A        KLIST
023200070523     C                   KFLD                    COMFIL
023300070523     C                   KFLD                    Kduc
023400070523     C                   KFLD                    KDrt
023500070523     C     KBLP5B        KLIST
023600070523     C                   KFLD                    COMFIL
023700070523     C                   KFLD                    BLPDUC
023800931130      * LETTURA FILE BOLLE. (PER RISALIRE DAL COLLO ALLA BOLLA)
023900950111     C     KBLP01        KLIST
024000930924     C                   KFLD                    BLTAAS
024100931008     C                   KFLD                    BLTLNP
024200930924     C                   KFLD                    BLTNRS
024300930924     C                   KFLD                    BLTNSP
024400020823     C     KBTP11        KLIST
024500020823     C                   KFLD                    BTTFLP
024600020823     C                   KFLD                    BTTAAS
024700950111     C                   KFLD                    BTTLNP
024800950111     C                   KFLD                    BTTNRS
024900950111     C                   KFLD                    BTTNSP
025000090911     C     FISPAK        KLIST
025100970128     C                   KFLD                    SIMFEL
025200000614     C                   KFLD                    LIN(XX)
025300970128     C                   KFLD                    DS1ADA
025400970204     C*
025500970204     C     KCLN          KLIST
025600970204     C                   KFLD                    SIMFEL
025700970204     C                   KFLD                    $TFA
025800970204     C                   KFLD                    ANNODA
025900970204     C                   KFLD                    MESEDA
026000970207     C*
026100970207     C     KANM          KLIST
026200970207     C                   KFLD                    WCAA
026300990518     C                   KFLD                    DS1ADA
026400000204     C     KBRV          KLIST
026500970505     C                   KFLD                    KNPG
026600970505     C                   KFLD                    ANMFLS
026700970505     C                   KFLD                    ANMLNA
026800970505     C                   KFLD                    ANMNRS
026900970505     C                   KFLD                    ANMSCN
027000970505     C                   KFLD                    SIMFEL
027100020823     C     KDTA          KLIST
027200020823     C                   KFLD                    SIMFEL
027300020823     C                   KFLD                    DS1ADA
027400021016
027500021016     c     Ktbe          Klist
027600021016     c                   Kfld                    kTbeCod
027700021016     c                   Kfld                    kTbeKe1
027800050829     c     KSPA2         Klist
027900050829     c                   Kfld                    DS1FEL
028000050829     c                   Kfld                    KLIn
028100050829     c     KSPA          Klist
028200050829     c                   Kfld                    DS1FEL
028300050829     c                   Kfld                    KLIn
028400050829     c                   Kfld                    Kdre
028500090415     c     KSTA          Klist
028600090415     c                   Kfld                    Ktrc
028700090415     c                   Kfld                    DS1FEL
028800090415     c                   Kfld                    Kfil
028900090415     c                   Kfld                    ds1ada
029000931130      *---------------------------------------------------------------*
029100931130      * ESEGUO OPERAZ. DA FARE UNA VOLTA SOLA, ALL'INIZIO
029200930927     C                   EXSR      INIZIO
029300950209      *
029400950209      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
029500950209     C                   EXSR      COMUNE
029600970129     C*
029700970129     C* CALL PGM PER CHIUDERE FILES
029800970129     C     DS1RIS        IFNE      'R'
029900970129     C                   MOVEL     'C'           D98TLA
030000970925     C                   CALL      'TNSD99R'
030100970925     C                   PARM                    DS99
030200970203     C                   END
030300931130      *
030400920930     C                   SETON                                        LR
030500950209      *
030600020212     C*****************************************************************
030700020212     C*  PARTE COMUNE STAMPE GIORNALIERE
030800020212     C*
030900020212     C*  . REPERIMENTO 'SIMFEL'
031000020212     C*  . CARICAMENTO '£1'
031100020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
031200020212     C*  . CODA DI STAMPA ('£OUTQ')
031300020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
031400020212     C*  . MODULO PER LA STAMPA ('£MOD')
031500020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
031600020212     C*****************************************************************
031700020212     C     COMUNE        BEGSR
031800020212     C*
031900020212     C*****************************************************************
032000020212     C*       RIEPILOGO INDICATORI:
032100020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
032200020212     C* 11    -           STAMPA PER 1L
032300020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
032400020212     C*****************************************************************
032500020212     C*
032600020212     C* Reperimento parametri aziendali
032700020212     C                   CALL      'X§PARUT'
032800020212     C                   PARM                    UT§DSE
032900020212     C                   MOVEL     RAGUT         RSUT             20
033000020212     C                   MOVEL     REC80         CNCR80
033100020212     C*
033200020212     C* Chiave di accesso per il file tabelle
033300020212     C     KTAB1         KLIST
033400020212     C                   KFLD                    CODUT
033500020212     C                   KFLD                    KCOD
033600020212     C                   KFLD                    KKEY
033700020212     C     KTAB2         KLIST
033800020212     C                   KFLD                    CODUT
033900020212     C                   KFLD                    KCOD
034000020212     C*
034100020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
034200020212     C*
034300020212     C* DETERMINO DATA E ORA DI ESECUZIONE
034400020212     C                   TIME                    WTIME            14 0
034500020212     C                   MOVE      WTIME         WDATE             8 0
034600020212     C                   MOVEL     WTIME         UTIME             6 0
034700040921     c*
034800040921     C                   CLEAR                   WLBDAT
034900040921     C                   MOVE      Wtime         G02DAT
035000040921     C                   CALL      'XSRDA8'
035100040921     C                   PARM                    WLBDAT
035200040921     C                   Z-ADD     G02INV        dateu             8 0
035300090914     c
035400090914     c* Accendo indicatore 10 de devo stampare anche tipo servizio H
035500090914     c                   seton                                        10
035600020212     C*
035700020418     C* CARICO TABELLA '£1'
035800020212     C                   CLEAR                   KPJBU
035900020212     C                   CLEAR                   DSUL06
036000020212     C                   MOVE      '£1'          D06COD
036100020212     C                   MOVEL     SIMFEL        D06KEY
036200050802     C                   MOVE      'S'           D06KEY
036300050802     C                   MOVEL     ds1ada        D06drf
036400020212     C                   MOVEL     DSUL06        KPJBU
036500020212     C                   CALL      'TRUL06R'
036600020212     C                   PARM                    KPJBA
036700020212     C                   MOVEL     KPJBU         DSUL06
036800020418     c                   movea     t06           sav_t06
036900020418
037000020418      * se in arrivo carico la £6
037100020418 b1  c                   if        ds1tbo = 'A'
037200020418     c                   clear                   wc
037300020418 b2  c                   do        30            wa
037400020422     c                   if        sav_t06(wa) = *zeros
037500020422     c                   leave
037600020422     c                   endif
037700020418     c                   clear                   kpjbu
037800020418     c                   clear                   dsul06
037900050802     C                   MOVEL     ds1ada        D06drf
038000020418     c                   eval      d06cod = '£6'
038100020418     c                   movel     sav_t06(wa)   d06key
038200050802     C                   MOVE      'S'           D06KEY
038300020418     c                   eval      d06esc = 'S'
038400020419     c                   movel     dsul06        kpjbu
038500020418     c                   call      'TRUL06R'
038600020418     c                   parm                    kpjba
038700020418     c                   movel     kpjbu         dsul06
038800020418 b3  c                   do        30            wb
038900020418     c                   if        t06(wb) = *zeros
039000020418     c                   leave
039100020418     c                   endif
039200020418     c                   add       1             wc
039300020418     c                   z-add     t06(wb)       l6(wc)
039400020418 e3  c                   enddo
039500020418 e2  c                   enddo
039600020212     C*
039700020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
039800020212     C                   Z-ADD     *ZERO         WB
039900020418 b2  C                   DO        29            WA
040000020418     C     sav_T06(WA)   LOOKUP    L6                                     30
040100020212     C  N30              ADD       1             WB
040200020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
040300020418 e2  C                   ENDDO
040400020418 x1  C                   ELSE
040500020418     C                   MOVEA     sav_T06       L1
040600020418 e1  C                   ENDIF
040700020212     C*
040800070103     C* ESCLUDO DA £1 LINEE NON consentite
040900020212     C                   MOVEA     L1            LIN
041000020212     C                   CLEAR                   L1
041100020212     C                   Z-ADD     *ZERO         WB                3 0
041200020212     C     1             DO        30            WPT               3 0
041300020212     C     LIN(WPT)      IFNE      0
041400020212     C                   CLEAR                   FNLSC4
041500020212     C                   Z-ADD     LIN(WPT)      IC4LIN
041600020212     C                   MOVE      'B'           IC4FLG
041700020212     C                   MOVEL     FNLSC4        KPJBU
041800020212     C                   CALL      'FNLSC4R1'
041900020212     C                   PARM                    KPJBA
042000050802     C                   PARM                    ds1ada
042100020212     C                   MOVEL     KPJBU         FNLSC4
042200020212     C     OC4ESE        IFEQ      'S'
042300020212     C                   ADD       1             WB
042400020212     C                   MOVE      LIN(WPT)      L1(WB)
042500020212     C                   ENDIF
042600020212     C                   ENDIF
042700020212     C                   ENDDO
042800070103     C*
042900070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
043000070103     C                   Z-ADD     *ZERO         WB                3 0
043100070103     C                   DO        29            WA                3 0
043200070103     C     L1(WA)        IFNE      *ZEROS
043300070103     C     L1(WA)        ANDNE     SIMFEL
043400070103     C                   ADD       1             WB
043500070103     C                   Z-ADD     L1(WA)        LV2(WB)
043600070103     C                   ENDIF
043700070103     C                   ENDDO
043800020212     C*
043900020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
044000020212     C*
044100020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
044200020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
044300020212     C* la stampa e' lanciata da menu
044400020212     C                   MOVEL     '3L'          KCOD
044500020212     C                   MOVEL     DS1PGM        KKEY
044600020212     C                   MOVE      DS1FPG        KKEY
044700020212     C     KTAB1         CHAIN     TABEL00F                           30
044800020212     C  N30              MOVEL     TBLUNI        DS3L
044900020212     C   30              MOVEL     'F'           §3LD1L
045000020212     C   30              MOVEL     ' '           §3LD2L
045100020212     C   30              MOVEL     '*STD    '    §3LDST
045200020212     C*
045300020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
045400020212     C                   MOVEL     §3LDST        £MOD             10
045500020212     C                   CLEAR                   LIN
045600020212     C*
045700020212     C* Verifico chi ha lanciato la stampa
045800020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
045900020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
046000020212     C* DS1FGS<>0  --->  si proviene da menu
046100020212     C     DS1FGS        IFEQ      0
046200020212     C*
046300020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
046400020212     C* lancio, voglio la stampa;  invece
046500020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
046600020212     C*
046700020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
046800020212     C* creare stampa per 1°lvl. (se stessa)
046900020212     C     DS1LNP        IFEQ      SIMFEL
047000020212     C*
047100020212     C* Il flag §3LD1L ha i seguenti significati:
047200020212     C*    ' ' = tutti i dati
047300020212     C*    '1' = solo filiale di 1' livello
047400020212     C     §3LD1L        IFEQ      '1'
047500020212     C                   SETON                                        11
047600020212     C                   END
047700020212     C*
047800020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
047900020212     C* nella stampa
048000020212     C*
048100020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
048200020212     C  N11              MOVEA     L1            LIN
048300020212     C   11              MOVEA     SIMFEL        LIN(1)
048400020212     C*
048500020212     C* IMPOSTO LA STAMPANTE:
048600020212     C* STAMPANTE TABULATI
048700020212     C                   MOVEL     '5P'          KCOD
048800020212     C                   MOVEL     'T'           W004A             4
048900020212     C                   MOVE      LIN(1)        W004A
049000020212     C                   MOVEL(P)  W004A         KKEY
049100020212     C     KTAB1         CHAIN     TABEL00F                           99
049200020212     C     *IN99         IFEQ      *OFF
049300020212     C                   MOVEL     TBLUNI        DS5P
049400020212     C                   MOVEL     §5POTQ        £OUTQ            10
049500020212     C                   END
049600020212     C*
049700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
049800020212     C                   EXSR      LEGGI
049900020212     C*
050000020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
050100020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
050200020212     C                   ELSE
050300020212     C*
050400020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
050500020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
050600020212     C*
050700020212     C* Il significato del fla §3LD2L e' il seguente:
050800020212     C*    ' ' = tutti i dati
050900020212     C*    'F' = per ogni 2' livello
051000020212     C     §3LD2L        IFEQ      'F'
051100020212     C                   SETON                                        14
051200020212     C                   ENDIF
051300020212     C*
051400020212     C                   Z-ADD     1             WB
051500020212     C                   MOVEA     *ZEROS        LIN
051600020212     C  N14              MOVEA     L1            LIN
051700020212     C*
051800020212     C* Lancio per tutti i 2° lvl
051900020212     C     DS1LNP        IFEQ      0
052000020212     C*
052100020212     C* LV2 contiene tutte le filiali di 2° lvl
052200020212     C     LV2(WB)       DOWNE     0
052300020212      *
052400020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
052500020212     C                   CLEAR                   FNLSC4
052600020212     C                   Z-ADD     LV2(WB)       IC4LIN
052700020212     C                   MOVEL     FNLSC4        KPJBU
052800020212     C                   CALL      'FNLSC4R1'
052900020212     C                   PARM                    KPJBA
053000050802     C                   PARM                    ds1ada
053100050802     C                   MOVEL     KPJBU         FNLSC4
053200020212      *
053300020212     C     OC4PT1        IFEQ      'S'
053400020212     C     §3LPT         ANDEQ     'N'
053500020212     C                   GOTO      NOLEGG
053600020212     C                   ENDIF
053700020212      *
053800020212     C     OC4EXP        IFEQ      'S'
053900020212     C     §3LEXP        ANDEQ     'N'
054000020212     C                   GOTO      NOLEGG
054100020212     C                   ENDIF
054200020212      *
054300020212     C     OC4DPD        IFEQ      'S'
054400020212     C     §3LDPD        ANDEQ     'N'
054500020212     C                   GOTO      NOLEGG
054600020212     C                   ENDIF
054700020212      *
054800020212     C     OC4BAR        IFEQ      'S'
054900020212     C     §3LBAR        ANDEQ     'N'
055000020212     C                   GOTO      NOLEGG
055100020212     C                   ENDIF
055200020212      *
055300020212     C     OC4FED        IFEQ      'S'
055400020212     C     §3LFED        ANDEQ     'N'
055500020212     C                   GOTO      NOLEGG
055600020212     C                   ENDIF
055700020212      *
055800020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
055900020212     C*  dati degli altri 2° lvl
056000020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
056100020212     C   14              MOVEL     LV2(WB)       LIN(1)
056200020212     C*
056300020212     C* IMPOSTO LA STAMPANTE
056400020212     C* STAMPANTE TABULATI
056500020212     C                   MOVEL     '5P'          KCOD
056600020212     C                   MOVEL     'T'           W004A             4
056700020212     C                   MOVE      LV2(WB)       W004A
056800020212     C                   MOVEL(P)  W004A         KKEY
056900020212     C     KTAB1         CHAIN     TABEL00F                           99
057000020212     C     *IN99         IFEQ      *OFF
057100020212     C                   MOVEL     TBLUNI        DS5P
057200020212     C                   MOVEL     §5POTQ        £OUTQ            10
057300020212     C                   END
057400020212     C*
057500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
057600020212     C                   EXSR      LEGGI
057700020212     C*
057800020212     C     NOLEGG        TAG
057900020212     C*
058000020212     C                   ADD       1             WB
058100020212     C                   ENDDO
058200020212     C*
058300020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
058400020212     C                   ELSE
058500020212     C*
058600020212     C* Lancio solo per la 2° lvl richiesta
058700020212     C   14              MOVEL     DS1LNP        LIN(1)
058800020212     C*
058900020212     C* STAMPANTE
059000020212     C* STAMPANTE TABULATI
059100020212     C                   MOVEL     '5P'          KCOD
059200020212     C                   MOVEL     'T'           W004A             4
059300020212     C                   MOVE      DS1LNP        W004A
059400020212     C                   MOVEL(P)  W004A         KKEY
059500020212     C     KTAB1         CHAIN     TABEL00F                           99
059600020212     C     *IN99         IFEQ      *OFF
059700020212     C                   MOVEL     TBLUNI        DS5P
059800020212     C                   MOVEL     §5POTQ        £OUTQ            10
059900020212     C                   END
060000020212     C*
060100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
060200020212     C                   EXSR      LEGGI
060300020212     C*
060400020212     C                   ENDIF
060500020212     C                   ENDIF
060600020212     C*
060700020212     C                   ELSE
060800020212     C*
060900020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
061000020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
061100020212     C     DS1LNP        IFEQ      0
061200020212     C                   MOVEA     L1            LIN
061300020212     C* ...altrimenti elaboro solo se stessa
061400020212     C                   ELSE
061500020212     C                   MOVEL     DS1LNP        LIN(1)
061600020212     C                   ENDIF
061700020212     C*
061800020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
061900020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
062000020212     C* PRENDO QUELLA PRESENTE IN KPJBA
062100020212     C     DS1FGS        IFEQ      SIMFEL
062200020212     C                   MOVEL     KQOTD         £OUTQ
062300030627     C**!!!              MOVEL     KQOTD         £OUTQF
062400020212     C                   ELSE
062500020212     C* STAMPANTE TABULATI
062600020212     C                   MOVEL     '5P'          KCOD
062700020212     C                   MOVEL     'T'           W004A             4
062800020212     C                   MOVE      DS1FGS        W004A
062900020212     C                   MOVEL(P)  W004A         KKEY
063000020212     C     KTAB1         CHAIN     TABEL00F                           99
063100020212     C     *IN99         IFEQ      *OFF
063200020212     C                   MOVEL     TBLUNI        DS5P
063300020212     C                   MOVEL     §5POTQ        £OUTQ            10
063400020212     C                   END
063500020212     C                   ENDIF
063600020212     C*
063700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
063800020212     C                   EXSR      LEGGI
063900020212     C                   ENDIF
064000020212     C*
064100020212     C                   ENDSR
064200931130      *****************************************************************
064300931130      * SUBROUTINE INIZIO: OPERAZIONI DA FARE UNA VOLTA SOLA ALL'INIZIO
064400931130      ****************************************************************
064500930927     C     INIZIO        BEGSR
064600950209      *
064700950209     C                   Z-ADD     1             CODUT
064800940104      *
064900941012      *  Stampa o ristampa ?
065000941012     C                   MOVE      'RISTAMPA'    STRIP             8
065100950404     C     DS1RIS        IFEQ      'S'
065200941012     C                   MOVE      'STAMPA  '    STRIP
065300941012     C                   ENDIF
065400941012      *
065500941012      * Scrivo, in stampa, se selezionata filiale o area
065600941012     C                   MOVE      *BLANKS       STRIP1            8
065700950210     C     DS1LNP        IFEQ      *ZERO
065800941012     C                   MOVEL     'AREA'        STRIP1
065900950210     C                   MOVE      DS1FEL        STRIP1
066000941012     C                   ELSE
066100960925     C                   MOVEL     'P.O.'        STRIP1
066200950210     C                   MOVE      DS1LNP        STRIP1
066300941012     C                   ENDIF
066400940429      *
066500941012      * Per sicurezza, se la data finale e' nulla metto quella iniziale
066600950210     C     DS1AAL        IFEQ      *ZERO
066700950210     C                   Z-ADD     DS1ADA        DS1AAL
066800970130     C                   Z-ADD     DS1GDA        DS1GAL
066900941012     C                   ENDIF
067000931130      *
067100931130      * TESTO SE E' STATA IMMESSA 'DATA A' ED E' <> DA 'DATA DA', SETON 03.
067200950210     C     DS1AAL        IFNE      0
067300950210     C     DS1AAL        ANDNE     DS1ADA
067400930927     C                   SETON                                        03
067500930927     C                   ENDIF
067600940509      *
067700941012      * Accendo il 36 se non desidero la stampa
067800950210     C     DS1RIS        COMP      *BLANKS                                36
067900940509      *
068000950111      * LEGGO LA DSQT1 PER IL RAPPORTO PESO/VOLUME
068100950209     C                   MOVEL     'QT'          KCOD
068200950209     C                   MOVEL     '1       '    KKEY
068300950209     C     KTAB1         CHAIN     TABEL00F                           32
068400950111     C  N32              MOVEL     TBLUNI        DSQT1
068500950111     C   32              CLEAR                   DSQT1
068600931130      *
068700931130      * REPERIMENTO TABELLA VARIABILI PGM
068800950209     C                   MOVEL     '3I'          KCOD
068900950209     C                   MOVEL     '1       '    KKEY
069000950209     C     KTAB1         CHAIN     TABEL00F                           32
069100930927     C  N32              MOVEL     TBLUNI        DS3I
069200930927     C   32              Z-ADD     20            §3IARR
069300950111      *
069400950111      * METTO IN SCHIERA LA TABELLA 3A PER EVITARE CHAIN SUCCESSIVE
069500950111     C                   Z-ADD     *ZERO         XX
069600950209     C                   MOVE      '3A'          KCOD
069700950209     C     KTAB2         SETLL     TABEL00F
069800950209     C     KTAB2         READE     TABEL00F                               32
069900950111     C     *IN32         DOWEQ     *OFF
070000950111     C                   ADD       1             XX
070100950209     C                   MOVEL     TBLKEY        E3A(XX)
070200950111     C                   MOVEL     TBLUNI        D3A(XX)
070300950209     C     KTAB2         READE     TABEL00F                               32
070400950111     C                   ENDDO
070500070523     C*
070600090605      * carico le descrizioni fisse dei tipi servizio
070700090605      * chiodo l'indice di schiera per ogni tipo servizio
070800090911      * 1=C - 2=E - 3=D  4=H
070900090605     c                   clear                   ds5e
071000090605     c                   clear                   wdtsp
071100090605     c                   eval      kcod = '5E'
071200090605     c     ktab2         setll     tabel00f
071300090605     c                   do        *hival
071400090605     c     ktab2         reade(n)  tabel00f
071500090605     c                   if        %eof(tabel00f)
071600090605     c                   leave
071700090605     c                   endif
071800090605     c                   eval      ds5e = tbluni
071900090605     c                   if        tblkey = 'C'
072000090605     c                   eval      wdtsp(01) = §5ed05
072100090605     c                   endif
072200090605     c                   if        tblkey = 'D'
072300090605     c                   eval      wdtsp(03) = §5ed05
072400090605     c                   endif
072500090605     c                   if        tblkey = 'E'
072600090605     c                   eval      wdtsp(02) = §5ed05
072700090605     c                   endif
072800090911     c                   if        tblkey = 'H'
072900090911     c                   eval      wdtsp(04) = §5ed05
073000090911     c                   endif
073100090605     c                   enddo
073200090914     c
073300090914     c
073400090605
073500930927     C                   ENDSR
073600930927     C*****************************************************************
073700930924     C* SUBROUTINE LEGGI: CICLO PRINCIPALE
073800930924     C*****************************************************************
073900920930     C     LEGGI         BEGSR
074000000615     C                   CLEAR                   SAVRIS
074100941012      *
074200941012      * Se sono nelle stampe giornaliere non voglio diciture
074300950210     C     DS1FGS        IFEQ      *ZERO
074400941012     C     SIMFEL        ANDNE     *ZERO
074500941012     C                   MOVE      *BLANKS       STRIP
074600941012     C                   MOVE      *BLANKS       STRIP1
074700941012     C                   ENDIF
074800941012      *
074900970129      * Cancello dal file statistiche i record della giornata che vado
075000000614      * a elabor. ELABORO SEMPRE TUTATA LA L1 E STAMPO LIN
075100000614      * D'ORA IN AVANTI IL PGM SARA' SEMRPE CHIAMATO SEPARATAMENTE
075200000614      * PERELABORARE E PER STAMPARE
075300950210     C     DS1RIS        IFNE      'R'
075400000614     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
075500000614     C                   CLEAR                   KPJBU
075600000614     C                   CLEAR                   DSUL06
075700000614     C                   MOVE      '£1'          D06COD
075800000614     C                   MOVEL     SIMFEL        D06KEY
075900050802     C                   MOVE      'S'           D06KEY
076000050802     C                   MOVEL     ds1ada        D06drf
076100000614     C                   MOVEL     DSUL06        KPJBU
076200000614     C                   CALL      'TRUL06R'
076300000614     C                   PARM                    KPJBA
076400000614     C                   MOVEL     KPJBU         DSUL06
076500000615     C**
076600000615     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
076700000615     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
076800000615     C*  CHE USO ALLA FINE
076900000615     C     DS1RIS        IFNE      ' '
077000000615     C                   MOVEA     LIN           SVLIN
077100000615     C                   MOVEL     DS1RIS        SAVRIS
077200000615     C                   CLEAR                   DS1RIS
077300000615     C                   ENDIF
077400000615     C*
077500000615     C                   MOVEA     T06           LIN
077600941012      *
077700941012     C                   Z-ADD     1             XX                3 0
077800000614     C     LIN(XX)       DOWGT     *ZERO
077900970129      *
078000090911     C     FISPAK        SETLL     FISPA01L
078100090911     C     FISPAK        READE     FISPA01L                               99
078200940509     C*
078300940509     C     *IN99         DOWEQ     *OFF
078400090911     C                   DELETE    FISPA000
078500090911     C     FISPAK        READE     FISPA01L                               99
078600941012     C                   ENDDO
078700940509      *
078800941012     C                   ADD       1             XX
078900941012     C                   ENDDO
079000941012     C                   ENDIF
079100950209      *
079200950209      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
079300950209     C                   MOVEA     £MOD          CMD(33)
079400950209     C                   MOVEA     £OUTQ         CMD(51)
079500931130      *
079600931130      * SE NON VA BENE L'OVERRIDE, IL PGM VA A FINE.
079700950209     C                   Z-ADD     82            LUNG             15 5
079800950209     C                   MOVEL     *BLANKS       COMMAN           82
079900931130     C                   MOVEA     CMD           COMMAN
080000950209     C                   CALL      'QCMDEXC'                            99
080100920930     C                   PARM                    COMMAN
080200920930     C                   PARM                    LUNG
080300940509     C   99              GOTO      ENDLEG
080400931130      *
080500020813     C* CONTATORE 'XD': USATO PER IL CONTEGGIO FILIALI PER SALTO PAGINA.
080600020813     C                   Z-ADD     1             XC                3 0
080700020813     C                   Z-ADD     0             XD                3 0
080800941012     C  N36              OPEN      PRTF198
080900931013     C****STAMPA TESTATA
081000941012     C  N36              EXCEPT    TESTA
081100970226     C                   SETOFF                                       OF
081200000615     C*
081300000615     C* SE CLEARATO DS1RIS ACCENDO INDICATORE
081400000615     C     DS1RIS        IFEQ      ' '
081500000615     C                   SETON                                        36
081600000615     C                   ENDIF
081700940509     C* SE IN LIN,1 C'E' IL 1°LVL, (08) ELABORO I TRANSITI.
081800940509     C     SIMFEL        LOOKUP    LIN                                    08
081900950403     C*
082000970228     C                   CLEAR                   WDIS
082100970505     C                   CLEAR                   WDIS88
082200970228     C*
082300950403     C     *IN08         IFEQ      *ON
082400970228     C     DS1RIS        ANDNE     'R'
082500950403     C*
082600990517     C* SOMMO LE ANOMALIE DI TIPO "056-DISGUIDI" AI TRANSITI
082700990517     C                   Z-ADD     56            WCAA
082800990518     C     KANM          SETLL     FNANM000
082900990518     C     KANM          READE     FNANM000                               32
083000950403     C*
083100950403     C     *IN32         DOWEQ     *OFF
083200021023      * Deve elaborare solo le anomalie con ANMFLE della L1
083300021023     c     AnmFle        Lookup    L1                                     99
083400021023     c                   If        Not *In99
083500021023     c                   GoTo      Rileggi
083600021023     c                   EndIf
083700970305     C     ANMAIE        IFEQ      'E'
083800950403     C                   ADD       1             WDIS              6 0
083900970505     C* CERCO LA RELATIVA SPUNTA PARTENZA
084000970505     C                   Z-ADD     1             KNPG
084100070112     C     KBRV          CHAIN     FNBRV09L                           32
084200970505     C  N32BRVNPS        IFEQ      88
084300970505     C     BRVNPS        OREQ      89
084400970505     C     BRVNPS        OREQ      98
084500970505     C     BRVNPS        OREQ      96
084600970502     C                   ADD       1             WDIS88            6 0
084700970502     C                   ENDIF
084800970505     C                   ENDIF
084900950403     C*
085000021023     c     Rileggi       Tag
085100021023
085200990518     C     KANM          READE     FNANM000                               32
085300950403     C                   ENDDO
085400950403     C                   ENDIF
085500950403     C*
085600950403     C*
085700941012      * Se richiesta 'S'=Stampa elaboro gli archivi, altrimenti leggo il file
085800090911      * statistiche (FISPA00F) e lo stampo direttamente
085900950210     C     DS1RIS        IFNE      'R'
086000970204     C* VERIFICO SU CALENDARIO SE GIORNO FESTIVO E LO MEMORIZZO
086100090911     C* (MI SERVE PER SAPERE IL TIPO RECORD DA MEMORIZZARE IN FISPA)
086200970204     C                   CLEAR                   $TFA
086300970204     C                   CLEAR                   WFES
086400970204     C     KCLN          CHAIN     AZCLN01L                           32
086500970204     C     *IN32         IFEQ      *OFF
086600970204     C                   MOVEA     CLNMAT        GME
086700970204     C                   MOVE      GME($GI)      WFES              1
086800970204     C                   END
086900930924     C*****
087000930924     C*CICLO PER TUTTE LE FILIALI (LINEE DI PARTENZA)
087100020813     C     LIN(XC)       DOWNE     0
087200931011     C*
087300020813     C                   ADD       1             XD
087400930924     C*VALORIZZO IL CAMPO CHIAVE ...LINEA DI PARTENZA
087500020813     C                   MOVEL     LIN(XC)       COMFIL
087600930928     C* REPERIMENTO DECODIFICA LINEA PARTENZA
087700930928     C     COMFIL        CHAIN     AZORG                              32
087800970203     C     *IN32         IFEQ      *OFF
087900970203     C     ORGDE5        IFNE      *BLANKS
088000970203     C                   MOVEL     ORGDE5        DESLNP
088100970203     C                   ELSE
088200970203     C                   MOVEL     ORGDES        DESLNP           10
088300970203     C                   END
088400970203     C                   ELSE
088500970203     C                   MOVEL     *BLANKS       DESLNP
088600970203     C                   END
088700931011     C*
088800930924     C**ELABORAZIONE FILIALI (LNP) CON MEMORIZZAZIONE DEI TOTALI
088900000831     C                   EXSR      ELALNP
089000000831     C**ELABORAZIONE ANOMALIE 105 PER LA LINEA INTERESSATA
089100000831     C                   EXSR      ELA105
089200930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
089300930924     C                   EXSR      TOTLNP
089400020813     C                   ADD       1             XC
089500930924     C                   ENDDO
089600930924     C*****
089700930924     C**CALCOLO % STAMPA DEI C/SERVIZI (CONTO SERVIZI)
089800930924     C                   EXSR      TOTCS
089900931013     C**CALCOLO % E STAMPA AREA(TUTTE LE FILIALI + C/SERVIZI).
090000930924     C                   EXSR      TOTARE
090100931006     C** PER 08 ELABORO I TRANSITI
090200931006     C   08              EXSR      ELATRA
090300930924     C**CALCOLI % E STAMPA DEI TRANSITI
090400931006     C   08              EXSR      TOTTRA
090500940103      *
090600941012     C                   ELSE
090700090911      * Ristampo dal file FISPA00F
090800941012     C                   EXSR      RISTAM
090900941012     C                   ENDIF
091000000615     C*
091100000615     C* SE DEVO ANCHE ELABORARE, LO FACCIO ORA
091200000615     C     SAVRIS        IFEQ      'S'
091300000615     C                   SETOFF                                       36
091400000615     C                   MOVEL     'R'           DS1RIS
091500000615     C                   MOVEA     SVLIN         LIN
091600000615     C                   EXSR      RISTAM
091700000615     C                   ENDIF
091800940103      *
091900930924     C**CALCOLI % E STAMPA DEI TOTALI GENERALI
092000941012      * SOLO SE STAMPATA ANCHE FILIALE DI 1° LIVELLO
092100000308      * OPPURE SE SONO IN RISTAMPA E HO STAMPATO LA RIGA DEI TRANSITI
092200000308     C     *IN08         IFEQ      *ON
092300000308     C     DS1RIS        OREQ      'R'
092400000308     C     TOTXKA        ANDGT     0
092500000308     C                   EXSR      TOTGEN
092600000308     C                   ENDIF
092700940405      *
092800941012      * Stampo la dicitura finale "N.B."
092900941012     C  N36              EXCEPT    FINSTA
093000931222      *
093100931222      *CHIUSURA FILE DI STAMPA
093200941012     C  N36              CLOSE     PRTF198
093300940223      *
093400941012      * Aggiorno la data ultima elaborazione
093500950210     C     DS1RIS        IFNE      'R'
093600021016
093700021016     c                   Eval      kTbeCod = 'SDF'
093800021016     c                   Movel(p)  Simfel        kTbeKe1
093900021016     c     Ktbe          Chain     Tntbe01l
094000021016     c                   If        %Found(Tntbe01l)
094100021016     c                   Movel     TbeUni        dSdf
094200021016     c                   If        §SdfParP = *Zeros or §SdfParP > Ds1Ada
094300021016     c                   Z-Add     Ds1Ada        §SdfParP
094400021016     c                   EndIf
094500021016     c                   If        §SdfParU < Ds1Ada
094600021016     c                   Z-Add     Ds1Ada        §SdfParU
094700021016     c                   EndIf
094800021016     c                   Movel     dSdf          TbeUni
094900021016     c                   Update    Tntbe000
095000021016     c                   EndIf
095100090415     C                   ENDIF
095200090415     c
095300090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
095400090415     c*  in FISTA
095500090415     c                   if        savris='S'
095600090415     c     ksta          delete    fista01l
095700941012     C                   ENDIF
095800940223      *
095900940509     C     ENDLEG        ENDSR
096000930924     C*****************************************************************
096100930924     C* SUBROUTINE ELALNP: ELABORAZIONE FILIALI (LINEE DI PARTENZA)
096200930924     C*****************************************************************
096300930924     C     ELALNP        BEGSR
096400950111     C*
096500931008     C                   Z-ADD     0             FLDTXT
096600931013     C*ELABORO COLLI
096700021023     C     KBLT30        SETLL     FNBLT38L
096800930924     C*
096900021023     C     KBLT30        READE     FNBLT38L                               31
097000930924     C* CICLO LETTURA COLLI
097100970130    1C     *IN31         DOWEQ     *OFF
097200931013     C* VEDERE I DATI  BOLLA SOLO AL CAMBIO BOLLA
097300970130    2C     DSTXT         IFNE      FLDTXT
097400950111     C                   Z-ADD     DSTXT         FLDTXT
097500950111     C     KBLP01        CHAIN     FNBLP01L                           32
097600931130      *
097700930927     C*SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
097800970130    3C     *IN32         IFEQ      *OFF
097900950111     C                   Z-ADD     1             WA                3 0
098000950111     C     BLPCBO        LOOKUP    E3A(WA)                                32
098100950111     C   32              MOVEL     D3A(WA)       DS3A
098200950111     C  N32              CLEAR                   DS3A
098300931130      *
098400970505     C* REPERIMENTO DATI BOLLA
098500931007     C                   EXSR      DTBOL
098600931007     C* REPERIMENTO DATI BOLLA, PER CALCOLO.
098700930927     C                   EXSR      DATBOL
098800931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN UN RECUPERO
098900970130   X3C                   ELSE
099000930924     C                   MOVEL     'R'           §3ARBL
099100970130    3C                   ENDIF
099200970130    2C                   ENDIF
099300930924     C*
099400931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
099500970130    2C     §3ARBL        IFNE      'R'
099600930924     C*ESECUZIONE SUBROUTINE DEI CALCOLI
099700930924     C                   EXSR      CALC
099800970130    2C                   ENDIF
099900930924     C*
100000021023     C     KBLT30        READE     FNBLT38L                               31
100100970130    1C                   ENDDO
100200101126
100300070523     c                   exsr      CALSPEDNEW
100400930924     C*
100500930924     C                   ENDSR
100600070523     C*****************************************************************
100700070523     C* CALCOLO LE SPEDIZIONI A MODO NUOVO CONSIDERANDO LA DATA
100800070523     C*   RITIRO MERCE
100900070523     C*****************************************************************
101000070523     C     CALSPEDNEW    BEGSR
101100070523     C***
101200070523     C*ELABORO SPEDIZIONI
101300070523     C**
101400070523     C** I° CICLO --> DUC = 0 Drt <= DATA ELABORAZIONE
101500070523     C**
101600070523     C                   MOVE      *ZEROS        KDUC
101700070523     C                   MOVE      *ZEROS        KDrt
101800070523     C     KBLP5A        SETGT     FNBLP05L
101900070523     C     KBLP5         READE     FNBLP05L                               31
102000070523    1C     *IN31         DOWEQ     *OFF
102100070523     C     BLPDrt        ANDLE     DS1ADA
102200070523      * TEST SE E' RECUPERO
102300070523     C                   Z-ADD     1             WA
102400070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
102500070523     C   32              MOVEL     D3A(WA)       DS3A
102600070523     C  N32              CLEAR                   DS3A
102700070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
102800070523    2C     §3ARBL        IFNE      'R'
102900070523     C     BLPNFV        ANDNE     99999
103000070523     C     BLPNFV        ANDNE     99998
103100070523     C     BLPCBO        ANDNE     '£ '
103200070525     C     BLPpdr        ANDgt     0
103300070523     C                   EXSR      CSPE
103400070523    2C                   ENDIF
103500070523     C     KBLP5         READE     FNBLP05L                               31
103600070523    1C                   ENDDO
103700070523     C**
103800070523     C** II° CICLO --> DUC = DATA ELABORAZIONE
103900070523     C**
104000070523     C                   Z-ADD     DS1ADA        KDUC
104100070523     C     KBLP5         SETLL     FNBLP05L
104200070523     C     KBLP5         READE     FNBLP05L                               31
104300070523    1C     *IN31         DOWEQ     *OFF
104400070523      * TEST SE E' RECUPERO
104500070523     C                   Z-ADD     1             WA
104600070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
104700070523     C   32              MOVEL     D3A(WA)       DS3A
104800070523     C  N32              CLEAR                   DS3A
104900070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
105000070523    2C     §3ARBL        IFNE      'R'
105100070525     C     BLPNFV        ANDNE     99999
105200070525     C     BLPNFV        ANDNE     99998
105300070525     C     BLPCBO        ANDNE     '£ '
105400070523     C                   EXSR      CSPE
105500070523    2C                   ENDIF
105600070523     C     KBLP5         READE     FNBLP05L                               31
105700070523    1C                   ENDDO
105800070523     C**
105900070523     C** III° CICLO --> DUC > DATA ELABORAZIONE; Drt <= DATA ELABORAZ.
106000070523     C**
106100070523     C                   Z-ADD     DS1ADA        BLPDUC
106200070523     C     KBLP5B        SETGT     FNBLP05L
106300070523     C     COMFIL        READE     FNBLP05L                               31
106400070523    1C     *IN31         DOWEQ     *OFF
106500070523     C*
106600070523    2C     BLPDrt        IFGT      DS1ADA
106700070523     C     KBLP5B        SETGT     FNBLP05L
106800070523     C                   GOTO      LEGBLP5
106900070523   x2C                   ELSE
107000070523      * TEST SE E' RECUPERO
107100070523     C                   Z-ADD     1             WA
107200070523     C     BLPCBO        LOOKUP    E3A(WA)                                32
107300070523     C   32              MOVEL     D3A(WA)       DS3A
107400070523     C  N32              CLEAR                   DS3A
107500070523     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
107600070523    3C     §3ARBL        IFNE      'R'
107700070525     C     BLPNFV        ANDNE     99999
107800070525     C     BLPNFV        ANDNE     99998
107900070525     C     BLPCBO        ANDNE     '£ '
108000070525     C     BLPpdr        ANDgt     0
108100070523     C                   EXSR      CSPE
108200070523    3C                   ENDIF
108300070523    2C                   ENDIF
108400070523     C     LEGBLP5       TAG
108500070523     C     COMFIL        READE     FNBLP05L                               31
108600070523    1C                   ENDDO
108700070523     C                   ENDSR
108800000831     C*****************************************************************
108900000831     C* ELABORO LE ANOMALIE 105 PER LA LINEA INTERESSATA
109000000831     C*****************************************************************
109100000831     C     ELA105        BEGSR
109200070523     C                   Z-ADD     105           WCAA
109300000831     C     KANM          SETLL     FNANM000
109400000831     C     KANM          READE     FNANM000                               32
109500000831     C*
109600000831     C     *IN32         DOWEQ     *OFF
109700000831     C* SE ANOMAIA CHIUSA VUOL DIRE CHE ADESSOHO LA BOLLA
109800000831     C     ANMDCH        IFEQ      0
109900040601     c     anmfle        andeq     simfel
110000000831     C* SE NON E' UGUALE ALLA LINEA DI PARTENZA, VEDO SE CONFERMABILE
110100000831     C*  DA ME
110200000831     C     ANMFLS        IFNE      COMFIL
110300000831     C* SE E' IN T06 NON MEMORIZZO, ALTRIMENTI MEMORIZZO PER SIMFEL
110400000831     C     ANMFLS        LOOKUP    T06                                    32
110500000831     C     *IN32         IFEQ      *OFF
110600000831     C     COMFIL        ANDEQ     SIMFEL
110700000831     C* NON SAPENDO IL TIPO SERVIZIO, SOMMO NELLA DISTRIBUZIONE
110800000831     C                   ADD       1             SKA(6)
110900000831     C                   ENDIF
111000000831     C*
111100000831     C                   ELSE
111200000831     C                   ADD       1             SKA(6)
111300000831     C                   ENDIF
111400000831     C                   ENDIF
111500000831     C*
111600000831     C     KANM          READE     FNANM000                               32
111700000831     C                   ENDDO
111800000831     C*
111900000831     C                   ENDSR
112000930924     C*****************************************************************
112100931007     C* SUBROUTINE DTBOL: DIFFERENZIA TRA FILIALE E C/SERVIZI
112200931013     C* ESEGUITA SOLO PER BOLLE NORMALI.
112300930927     C*****************************************************************
112400931007     C     DTBOL         BEGSR
112500000831     C*
112600970129     C* MEMORIZZO SE E' DISK B O ALTRI
112700970129     C     BLPSOP        IFEQ      '§'
112800970129     C     BLPNRS        ANDGT     *ZEROS
112900970129     C     BLPSOP        OREQ      '%'
113000970129     C     BLPNRS        ANDGT     *ZEROS
113100970129     C                   SETON                                        05
113200970129     C                   ELSE
113300970129     C                   SETOFF                                       05
113400970129     C                   END
113500090914     C
113600090914     C* FISSO GLI INDICI DI SKIERA
113700090914     C                   EXSR      INDSKIERA
113800970129     C*
113900931007     C                   ENDSR
114000090914     C************************************************************
114100090914     C* FISSO GLI INDICI DI SKIERA
114200090914     C*****************************************************************
114300090914     C     IndSkiera     BEGSR
114400090914     C*TEST SE E' C/SERVIZI O FILIALE
114500090914     C     §3ARBL        IFEQ      'C'
114600090914     C                   SETON                                        04
114700090914     C                   ELSE
114800090914     C                   SETOFF                                       04
114900090914     C                   ENDIF
115000090914     C*
115100090914     C* SE TIPO SPEDIZIONE E' H 10 30
115200090914     C                   SELECT
115300090914     C     BLPTSP        WHENEQ    'H'
115400090914     C*  C /SERVIZI
115500090914     C   04              Z-ADD     4             X
115600090914     C*
115700090914     C  N04              Z-ADD     9             X
115800090914     C
115900090914     C* SE TIPO SPEDIZIONE E' PRIORITY
116000090914     C     BLPTSP        WHENEQ    'E'
116100090914     C*  C /SERVIZI
116200090914     C   04              Z-ADD     2             X
116300090914     C*
116400090914     C  N04              Z-ADD     7             X
116500090914     C
116600090914     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
116700090914     C     BLPTSP        WHENEQ    'D'
116800090914     C*  C/SERVIZI
116900090914     C   04              Z-ADD     3             X
117000090914     C*
117100090914     C  N04              Z-ADD     8             X
117200090914     C
117300090914     C* SE TIPO SPEDIZIONE E' EXPRESS
117400090914     c                   other
117500090914     C*
117600090914     C*  C/SERVIZI
117700090914     C   04              Z-ADD     1             X                 2 0
117800090914     C*
117900090914     C  N04              Z-ADD     6             X
118000090914     C*
118100090914     C                   ENDsl
118200090914     C*
118300090914      * Preparo l'indice per c/servizio di filiale (dal 7 al 9), per le schiere
118400090914     C   04X             ADD       10            Y                 2 0
118500090914     c
118600090914     c                   ENDSR
118700931007     C************************************************************
118800931007     C* SUBROUTINE DATBOL: REPERIMENTO DATI BOLLA PER CALCOLI.
118900931007     C*****************************************************************
119000931007     C     DATBOL        BEGSR
119100021213     ***C*
119200021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
119300021213     ***C     BLPVLF        DIV(H)    BLPNCL        VOLUM            12 4
119400021213     ***C*
119500021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
119600021213     ***C     BLPPKF        DIV(H)    BLPNCL        PESCOL            8 1
119700021220      *
119800021220      * Verifico i dati misurati con la macchina smistacolli
119900021220      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
120000021220      * e calcolo il VOLUME unitario medio per collo da usare se non
120100021220      * tutti i colli sono stati volumati dalla smistacolli
120200021220      *
120300030213      **** Verifico se sono stati volumati tutti i colli
120400030213     c***                clear                   Wsivol
120500021220     c                   clear                   Wvolu
120600050111     c                   if        BLPncr = BLPncl
120700021220     c                   z-add     BLPvlc        Wvolu
120800030213     c***                eval      Wsivol =  'S'
120900021220   x1c                   else
121000021220if  2c                   if        BLPvlc >= BLPvlf
121100021220     c                   z-add     BLPvlc        Wvolu
121200021220   x2c                   else
121300021220     c                   z-add     BLPvlf        Wvolu
121400021220e   2c                   endif
121500021220e   1c                   endif
121600021220      *
121700021220      * Calcolo il volume medio
121800021220     c     Wvolu         div(H)    BLPncl        wVOLUM
121900021220      *
122000021220      * Verifico i dati misurati con la macchina smistacolli
122100021220      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
122200021220      * e calcolo il PESO   unitario medio per collo da usare se non
122300021220      * tutti i colli sono stati pesati   dalla smistacolli
122400021220      *
122500030213      **** Verifico se sono stati pesati   tutti i colli
122600030213     c***                clear                   Wsipes
122700021220     c                   clear                   Wpeso
122800050111     c                   if        BLPncp = BLPncl
122900021220     c                   z-add     BLPpkc        Wpeso
123000030213     c***                eval      Wsipes =  'S'
123100021220   c1c                   else
123200021220if  2c                   if        BLPpkc >= BLPpkf
123300021220     c                   z-add     BLPpkc        Wpeso
123400021220   x2c                   else
123500021220     c                   z-add     BLPpkf        Wpeso
123600021220e   2c                   endif
123700021220e   1c                   endif
123800021220      *
123900021220      * Calcolo il peso medio
124000021220     c     Wpeso         div(H)    BLPncl        wPESCOL
124100940328     C*
124200930927     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
124300021213     ***C     BLPPKF        IFLE      3500
124400021213     C     Wpeso         IFLE      3500
124500930927     C                   SETON                                        06
124600930927     C                   ELSE
124700930927     C                   SETOFF                                       06
124800930927     C                   ENDIF
124900930927     C*
125000931006     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
125100021213     ***C                   Z-ADD     BLPPKF        PESTAX            8 0
125200021213     C                   z-add     Wpeso         PESTAX
125300930927     C*
125400930927     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
125500021213     ***C     BLPVLF        MULT      §QTKPM        COMPKT
125600021213     C     Wvolu         MULT      §QTKPM        COMPKT
125700930927     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
125800930927     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
125900930927     C     COMPKT        IFGT      PESTAX
126000930927     C                   Z-ADD     COMPKT        PESTAX
126100930927     C                   END
126200930930     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
126300930927     C                   Z-ADD     0             COMPKT            8 0
126400930927     C     PESTAX        DIV       §3IARR        COMPKT
126500930927     C                   MVR                     RESTO             3 0
126600930927     C     RESTO         IFGT      0
126700930927     C     COMPKT        MULT      §3IARR        PESTAX
126800930927     C                   ADD       §3IARR        PESTAX
126900930930     C                   ENDIF
127000021213     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
127100021213     C     PESTAX        DIV(H)    BLPNCL        PESTAS
127200930927     C*
127300930927     C                   ENDSR
127400930927     C****************************************************************
127500930929     C* SUBROUTINE CALC:
127600930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
127700930927     C*****************************************************************
127800930927     C     CALC          BEGSR
127900930927     C*
128000930927     C******COLLI
128100930927     C                   ADD       1             SKA(X)
128200941012     C   04              ADD       1             SKA(Y)
128300930927     C*
128400930927     C******COLLI NON SPUNTATI IN PARTENZA: 88.
128500930927     C     BLTNPP        IFEQ      88
128600950328     C     BLTNPP        OREQ      89
128700970430     C     BLTNPP        OREQ      98
128800970430     C     BLTNPP        OREQ      96
128900090914     c
129000000831     C* SOLO PER LE BOLLE POSTE, CONTO ANCHE I COLLI CON NPS=90
129100090914     C**** LNPPT         OREQ      'S'
129200090914     C**** BLTNPP        ANDEQ     90
129300970129     C* DISK B
129400970129     C     *IN05         IFEQ      *ON
129500970129     C*SE (FILIALE) C/SERVIZI
129600970129     C   04              ADD       1             PB88CS            7 0
129700970129     C   04              ADD       1             PB88              7 0
129800970129     C*SE FILIALE
129900970129     C  N04              ADD       1             PB88F             7 0
130000970129     C                   ELSE
130100970129     C* NO DISK B
130200970129     C*
130300930927     C*SE (FILIALE) C/SERVIZI
130400930927     C   04              ADD       1             PI88CS            7 0
130500941012     C   04              ADD       1             PS88              7 0
130600930927     C*SE FILIALE
130700930927     C  N04              ADD       1             PI88              7 0
130800970129     C                   END
130900930927     C                   ENDIF
131000950622     C******COLLI PISTOLA 87 (EX 88)
131100950622     C     BLTNPP        IFEQ      87
131200970129     C* DISK B
131300970129     C     *IN05         IFEQ      *ON
131400970129     C*SE (FILIALE) C/SERVIZI
131500970129     C   04              ADD       1             PB87CS            7 0
131600970129     C   04              ADD       1             PB87              7 0
131700970129     C*SE FILIALE
131800970129     C  N04              ADD       1             PB87F             7 0
131900970129     C                   ELSE
132000970129     C* NO DISK B
132100970129     C*
132200950622     C*SE (FILIALE) C/SERVIZI
132300950622     C   04              ADD       1             PI87CS            7 0
132400950622     C   04              ADD       1             PS87              7 0
132500950622     C*SE FILIALE
132600950622     C  N04              ADD       1             PI87              7 0
132700970129     C                   END
132800950622     C                   ENDIF
132900930927     C*
133000930927     C******PESO
133100930927     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
133200030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
133300030213      * Peso medio
133400030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
133500030213if  1c***                if        Wsipes = 'S'
133600030213     c***                z-add(H)  BLTpuc        PESCOL
133700030213   x1c***                else
133800021220     c                   z-add     Wpescol       PESCOL
133900030213e   1c***                endif
134000930927     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
134100930927     C   06              ADD       PESCOL        SKD(X)
134200941012     C   06
134300941012     CAN 04              ADD       PESCOL        SKD(Y)
134400970129     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
134500970129     C  N06              ADD       PESCOL        SKE(X)
134600970129     C  N06
134700970129     CAN 04              ADD       PESCOL        SKE(Y)
134800930927     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
134900930927     C                   ADD       PESCOL        SKF(X)
135000941012     C   04              ADD       PESCOL        SKF(Y)
135100930927     C*
135200930927     C******PESO TASSABILE
135300931007     C  N04
135400931007     CAN 06              ADD       PESTAS        PETS              9 1
135500931130      *
135600931130      ***** VOLUME
135700021220     *** * Se nel collo c'e' il volume utilizzo quello, altrimenti quello
135800021220     *** *  generico
135900021213     ***C     BLTVUC        IFNE      *ZERO
136000021213     ***C                   Z-ADD(H)  BLTVUC        WVOLUM
136100021213     ***C                   ELSE
136200021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
136300021213     ***C                   ENDIF
136400021220     ***C                   ADD       WVOLUM        SKI(X)
136500021220     ***C   04              ADD       WVOLUM        SKI(Y)
136600021220     *** *
136700021220      * VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
136800030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
136900030213      * Volume medio
137000030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
137100030213if  1c***                if        Wsivol = 'S'
137200030213     c***                z-add     BLTvuc        Volum
137300030213   x1c***                else
137400021220     c                   z-add     Wvolum        Volum
137500030213e   1c***                endif
137600021220     c                   add       VOLUM         SKI(X)
137700021220     c   04              add       VOLUM         SKI(Y)
137800931130      *
137900930927     C                   ENDSR
138000930927     C****************************************************************
138100931007     C* SUBROUTINE CSPE:  CALCOLO SPEDIZIONI
138200930929     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
138300930927     C*****************************************************************
138400931007     C     CSPE          BEGSR
138500931006     C*
138600931006     C*FISSO GLI INDICI DI SCHIERA....
138700090914     c                   exsr      IndSkiera
138800970228     C*
138900970206     C* Richiamo pgm per calcolo affidabilità
139000970303     C     BLPDCE        IFEQ      *ZEROS
139100970303     C     BLPDUC        ORNE      DS1ADA
139200970206     C                   MOVEL     ' '           D98TLA
139300970206     C                   MOVEL     'P'           D98TBO
139400970206     C                   MOVEL     BLPAAS        D98AAS
139500970206     C                   MOVEL     BLPLNP        D98LNP
139600970206     C                   MOVEL     BLPNRS        D98NRS
139700970206     C                   MOVEL     BLPNSP        D98NSP
139800970310     C                   Z-ADD     DS1ADA        D98DRF
139900970310     C                   Z-ADD     2359          D98ORF
140000970925     C                   CALL      'TNSD99R'
140100970925     C                   PARM                    DS99
140200970303     C                   ELSE
140300970303     C                   Z-ADD     BLPNPI        D98NPI
140400970303     C                   END
140500970206     C*
140600930927     C******SPEDIZIONI
140700970206    1C     BLPDUC        IFEQ      DS1ADA
140800970206     C*  PARTITO
140900930927     C                   ADD       1             SKL(X)
141000941012     C   04              ADD       1             SKL(Y)
141100970206     C*  PARTITO FUORI AFFIDABILITA'
141200090911     C* (SKO LO CONTO SOLO PER MEMORIZZARLO NEL FILE FISPA)
141300970206    2C     D98NPI        IFGT      0
141400970206     C                   ADD       1             SKO(X)
141500970206     C   04              ADD       1             SKO(Y)
141600970206     C                   ADD       1             SKN(X)
141700970206     C   04              ADD       1             SKN(Y)
141800970206    2C                   END
141900970206   X1C                   ELSE
142000970206     C* A TERRA FUORI AFFIDABILITA'
142100090911     C* (SKP LO CONTO SOLO PER MEMORIZZARLO NEL FILE FISPA)
142200970206    2C     D98NPI        IFGE      0
142300970206     C                   ADD       1             SKP(X)
142400970206     C   04              ADD       1             SKP(Y)
142500970206     C                   ADD       1             SKN(X)
142600970206     C   04              ADD       1             SKN(Y)
142700970206    2C                   END
142800970206    1C                   END
142900970129     C*  DA PARTIRE
143000970206     C                   MOVE      *BLANKS       COMDAP            1
143100970228     C* La distribuzione a terra del giorno non spareggiata non viene
143200970228     C* conteggiata nel "Da partire"
143300970303     C     BLPDUC        IFEQ      *ZEROS
143400970228     C     BLPDPC        ANDEQ     0
143500970206     C     D98NPI        ANDLT     0
143600970228     C*
143700970303     C     BLPDUC        OREQ      *ZEROS
143800970228     C     BLPDPC        ANDGT     DS1ADA
143900970228     C     D98NPI        ANDLT     0
144000970228     C*
144100970303     C     BLPDUC        ORGT      DS1ADA
144200970228     C     BLPDPC        ANDGT     DS1ADA
144300970228     C     D98NPI        ANDLT     0
144400970228     C*
144500970206     C                   MOVE      'N'           COMDAP
144600970206     C                   END
144700970206     C*
144800970206     C     COMDAP        IFEQ      *BLANKS
144900970129     C                   ADD       1             SKM(X)
145000970129     C   04              ADD       1             SKM(Y)
145100970206     C                   END
145200970228     C*
145300930927     C                   ENDSR
145400930927     C****************************************************************
145500930927     C* SUBROUTINE TOTLNP:
145600930927     C**CALCOLO %, CALCOLO TOTALI FILIALE, STAMPA DETTAGLIO FILIALE,
145700930927     C*****************************************************************
145800930927     C     TOTLNP        BEGSR
145900950111     C*
146000930927     C***CALCOLO TOTALI COLLI
146100930927     C*TOTALE COLLI
146200090914     C     SKA(6)        ADD       SKA(7)        SKA456            7 0
146300090914     C                   ADD       SKA(8)        SKA456
146400090914     C                   ADD       SKA(9)        SKA456
146500930927     C**** **** **** **** **** **** **** **** ****
146600930927     C*
146700970129     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
146800930930     C     SKA456        IFNE      0
146900970129     C* DISK B
147000970203     C     PB88F         DIV(H)    SKA456        $XX               7 4
147100970129     C     $XX           MULT      100           PC88B             5 2
147200970129     C* ALTRI
147300970129     C     PI88          DIV(H)    SKA456        $XX
147400970129     C     $XX           MULT      100           PC88              5 2
147500970129     C* TOTALE
147600970129     C     PB88F         ADD       PI88          PI88T             7 0
147700970129     C     PI88T         DIV(H)    SKA456        $XX
147800970129     C     $XX           MULT      100           PC88T             5 2
147900930930     C                   ENDIF
148000931005     C*
148100970129     C                   CLEAR                   PER3T
148200970129     C     PB88F         IFNE      0
148300970129     C                   MOVE      '%'           PER3B             1
148400970129     C                   MOVE      '%'           PER3T             1
148500970129     C                   ELSE
148600970129     C                   MOVE      *BLANKS       PER3B
148700970129     C                   ENDIF
148800970129     C*
148900931005     C     PI88          IFNE      0
149000931005     C                   MOVE      '%'           PER3              1
149100970129     C                   MOVE      '%'           PER3T             1
149200931005     C                   ELSE
149300931005     C                   MOVE      *BLANKS       PER3
149400931005     C                   ENDIF
149500950622     C*
149600950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
149700950622     C     SKA456        IFNE      0
149800970129     C* DISK B
149900970129     C     PB87F         DIV(H)    SKA456        $XX
150000970129     C     $XX           MULT      100           PC87B             5 2
150100970129     C* ALTRI
150200950622     C     PI87          DIV(H)    SKA456        $XX
150300950622     C     $XX           MULT      100           PC87              5 2
150400970129     C* TOTALI
150500970129     C     PB87F         ADD       PI87          PI87T             7 0
150600970129     C     PI87T         DIV(H)    SKA456        $XX
150700970129     C     $XX           MULT      100           PC87T             5 2
150800950622     C                   ENDIF
150900950622     C*
151000970129     C                   CLEAR                   PER34T
151100950622     C     PI87          IFNE      0
151200950622     C                   MOVE      '%'           PER34             1
151300970129     C                   MOVE      '%'           PER34T            1
151400950622     C                   ELSE
151500950622     C                   MOVE      *BLANKS       PER34
151600950622     C                   ENDIF
151700970129     C     PB87F         IFNE      0
151800970129     C                   MOVE      '%'           PER34B            1
151900970129     C                   MOVE      '%'           PER34T            1
152000970129     C                   ELSE
152100970129     C                   MOVE      *BLANKS       PER34B
152200970129     C                   ENDIF
152300930927     C**** **** **** **** **** **** **** **** ****
152400930927     C***CALCOLO TOTALI PESO
152500930927     C*TOTALE PESO FINO A 35 QUINTALI
152600090914     C     SKD(6)        ADD       SKD(7)        SKD456            8 1
152700090914     C                   ADD       SKD(8)        SKD456
152800090914     C                   ADD       SKD(9)        SKD456
152900930927     C*TOTALE PESO OLTRE  35 QUINTALI
153000090914     C     SKE(6)        ADD       SKE(7)        SKE456            8 1
153100090914     C                   ADD       SKE(8)        SKE456
153200090914     C                   ADD       SKE(9)        SKE456
153300930927     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
153400090914     C     SKF(6)        ADD       SKF(7)        SKF456            8 1
153500090914     C                   ADD       SKF(8)        SKF456
153600090914     C                   ADD       SKF(9)        SKF456
153700930927     C**** **** **** **** **** **** **** **** ****
153800970129     C***CALCOLO TOTALE VOLUME
153900930927     C*TOTALE VOLUMI
154000090914     C     SKI(6)        ADD       SKI(7)        SKI456            9 4
154100090914     C                   ADD       SKI(8)        SKI456
154200090914     C                   ADD       SKI(9)        SKI456
154300930927     C**** **** **** **** **** **** **** **** ****
154400930927     C****CALCOLO % E TOTALI SPEDIZIONI
154500970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI CLEARO CAMPI DEL
154600970226     C* DA PARTIRE E FUORI AFFIDABILITA'
154700970226     C     WFES          IFEQ      'F'
154800090914     C     SKA(6)        ANDEQ     *ZEROS
154900090914     C     SKA(7)        ANDEQ     *ZEROS
155000090914     C     SKA(8)        ANDEQ     *ZEROS
155100090914     C     SKA(9)        ANDEQ     *ZEROS
155200090914     C                   CLEAR                   SKM(6)
155300090914     C                   CLEAR                   SKM(7)
155400090914     C                   CLEAR                   SKM(8)
155500090914     C                   CLEAR                   SKM(9)
155600090914     C                   CLEAR                   SKN(6)
155700090914     C                   CLEAR                   SKN(7)
155800090914     C                   CLEAR                   SKN(8)
155900090914     C                   CLEAR                   SKN(9)
156000090914     C                   CLEAR                   SKP(6)
156100090914     C                   CLEAR                   SKP(7)
156200090914     C                   CLEAR                   SKP(8)
156300090914     C                   CLEAR                   SKP(9)
156400970226     C                   END
156500930927     C***CALCOLO TOTALI SPEDIZIONI
156600970129     C*TOTALE SPEDIZIONI PARTITE
156700090914     C     SKL(6)        ADD       SKL(7)        SKL456            7 0
156800090914     C                   ADD       SKL(8)        SKL456
156900090914     C                   ADD       SKL(9)        SKL456
157000970305     C*TOTALE SPEDIZIONI DA PARTIRE
157100090914     C     SKM(6)        ADD       SKM(7)        SKM456            7 0
157200090914     C                   ADD       SKM(8)        SKM456
157300090914     C                   ADD       SKM(9)        SKM456
157400930927     C**** **** **** **** **** **** **** **** ****
157500930928     C****STAMPA SINGOLA FILIALE :DETTAGLIO E TOTALE.
157600970226     C*
157700931011     C     *INOF         IFEQ      *ON
157800941012     C  N36              EXCEPT    TESTA
157900020813     C                   Z-ADD     1             XD
158000931011     C                   ENDIF
158100931011     C*
158200941012     C  N36              EXCEPT    DETFIL
158300950210     C     DS1RIS        IFNE      'R'
158400941012     C                   EXSR      WRIFIL
158500941012     C                   ENDIF
158600930928     C**** **** **** **** **** **** **** **** ****
158700930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE, IN CAMPI AREA
158800000616     C** SOLO SE DEVO STAMPARE
158900000616     C     DS1RIS        IFNE      ' '
159000930929     C                   ADD       SKA456        AKA123
159100090914     C                   ADD       SKA(6)        AKA(1)
159200090914     C                   ADD       SKA(7)        AKA(2)
159300090914     C                   ADD       SKA(8)        AKA(3)
159400090914     C                   ADD       SKA(9)        AKA(4)
159500930929     C*
159600930929     C                   ADD       PI88          ARPI88            7 0
159700970129     C                   ADD       PB88F         ARPB88            7 0
159800970130     C                   ADD       PI88T         ARP88T            7 0
159900950622     C                   ADD       PI87          ARPI87            7 0
160000970130     C                   ADD       PB87F         ARPB87            7 0
160100970130     C                   ADD       PI87T         ARP87T            7 0
160200930929     C*
160300930929     C                   ADD       SKD456        AKD123
160400090914     C                   ADD       SKD(6)        AKD(1)
160500090914     C                   ADD       SKD(7)        AKD(2)
160600090914     C                   ADD       SKD(8)        AKD(3)
160700090914     C                   ADD       SKD(9)        AKD(4)
160800930929     C*
160900930929     C                   ADD       SKE456        AKE123
161000090914     C                   ADD       SKE(6)        AKE(1)
161100090914     C                   ADD       SKE(7)        AKE(2)
161200090914     C                   ADD       SKE(8)        AKE(3)
161300090914     C                   ADD       SKE(9)        AKE(4)
161400930929     C*
161500930929     C                   ADD       SKF456        AKF123
161600090914     C                   ADD       SKF(6)        AKF(1)
161700090914     C                   ADD       SKF(7)        AKF(2)
161800090914     C                   ADD       SKF(8)        AKF(3)
161900090914     C                   ADD       SKF(9)        AKF(4)
162000930929     C*
162100931007     C                   ADD       PETS          ARPETS            9 1
162200930929     C*
162300930929     C                   ADD       SKI456        AKI123
162400090914     C                   ADD       SKI(6)        AKI(1)
162500090914     C                   ADD       SKI(7)        AKI(2)
162600090914     C                   ADD       SKI(8)        AKI(3)
162700090914     C                   ADD       SKI(9)        AKI(4)
162800970129     C*
162900930929     C                   ADD       SKL456        AKL123
163000090914     C                   ADD       SKL(6)        AKL(1)
163100090914     C                   ADD       SKL(7)        AKL(2)
163200090914     C                   ADD       SKL(8)        AKL(3)
163300090914     C                   ADD       SKL(9)        AKL(4)
163400930929     C*
163500930929     C                   ADD       SKM456        AKM123
163600090914     C                   ADD       SKM(6)        AKM(1)
163700090914     C                   ADD       SKM(7)        AKM(2)
163800090914     C                   ADD       SKM(8)        AKM(3)
163900090914     C                   ADD       SKM(9)        AKM(4)
164000930929     C*
164100090914     C                   ADD       SKN(6)        AKN(1)
164200090914     C                   ADD       SKN(7)        AKN(2)
164300090914     C                   ADD       SKN(8)        AKN(3)
164400090914     C                   ADD       SKN(9)        AKN(4)
164500000616     C                   ENDIF
164600930929     C*
164700930929     C**** **** **** **** **** **** **** **** ****
164800930929     C****PULIZIA CAMPI FILIALE (DI TOTALE E DI DETTAGLIO)
164900930929     C*CAMPI DI TOTALE
165000930929     C                   Z-ADD     0             SKA456
165100930929     C                   Z-ADD     0             PI88
165200941012     C                   Z-ADD     0             PS88
165300930929     C                   Z-ADD     0             PC88
165400970129     C                   Z-ADD     0             PB88F
165500970129     C                   Z-ADD     0             PB88
165600970129     C                   Z-ADD     0             PC88B
165700970130     C                   Z-ADD     0             PI88T
165800970129     C                   Z-ADD     0             PC88T
165900950622     C                   Z-ADD     0             PI87
166000950622     C                   Z-ADD     0             PS87
166100950622     C                   Z-ADD     0             PC87
166200970129     C                   Z-ADD     0             PB87F
166300970129     C                   Z-ADD     0             PB87
166400970129     C                   Z-ADD     0             PC87B
166500970130     C                   Z-ADD     0             PI87T
166600970129     C                   Z-ADD     0             PC87T
166700930929     C                   Z-ADD     0             SKD456
166800930929     C                   Z-ADD     0             SKE456
166900930929     C                   Z-ADD     0             SKF456
167000930929     C                   Z-ADD     0             PETS
167100930929     C                   Z-ADD     0             SKI456
167200930929     C                   Z-ADD     0             SKL456
167300930929     C                   Z-ADD     0             SKM456
167400930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
167500930929     C*PER LA STAMPA).
167600090914     C                   MOVEA     *ZEROS        SKA(6)
167700930929     C*
167800090914     C                   MOVEA     *ZEROS        SKD(6)
167900930929     C*
168000090914     C                   MOVEA     *ZEROS        SKE(6)
168100930929     C*
168200090914     C                   MOVEA     *ZEROS        SKF(6)
168300930929     C*
168400090914     C                   MOVEA     *ZEROS        SKI(6)
168500930929     C*
168600090914     C                   MOVEA     *ZEROS        SKL(6)
168700090914     C*
168800090914     C                   MOVEA     *ZEROS        SKM(6)
168900930929     C*
169000090914     C                   MOVEA     *ZEROS        SKN(6)
169100090914     C                   MOVEA     *ZEROS        SKO(6)
169200090914     C                   MOVEA     *ZEROS        SKP(6)
169300930929     C*
169400930927     C                   ENDSR
169500930929     C****************************************************************
169600930929     C* SUBROUTINE TOTCS:
169700930929     C**CALCOLO %, CALCOLO TOTALI C/SERVIZI, STAMPA DETTAGLIO C/SERVIZI,
169800930929     C*****************************************************************
169900930929     C     TOTCS         BEGSR
170000950111     C*
170100930929     C***CALCOLO TOTALI COLLI
170200930929     C*TOTALE COLLI
170300930929     C     SKA(1)        ADD       SKA(2)        SKA123            7 0
170400930929     C                   ADD       SKA(3)        SKA123
170500090914     C                   ADD       SKA(4)        SKA123
170600930929     C**** **** **** **** **** **** **** **** ****
170700970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
170800930930     C     SKA123        IFNE      0
170900970130     C* DISK B
171000970130     C     PB88CS        DIV(H)    SKA123        $XX
171100970130     C     $XX           MULT      100           PC88CB            5 2
171200970130     C* ALTRI
171300931005     C     PI88CS        DIV(H)    SKA123        $XX
171400931005     C     $XX           MULT      100           PC88CS            5 2
171500970130     C* TOTALE
171600970130     C     PB88CS        ADD       PI88CS        P88CST            7 0
171700970130     C     P88CST        DIV(H)    SKA123        $XX
171800970130     C     $XX           MULT      100           PC88CT            5 2
171900930930     C                   ENDIF
172000931005     C*
172100970130     C                   CLEAR                   PER3T
172200970130     C     PB88CS        IFNE      0
172300970130     C                   MOVE      '%'           PER3B
172400970130     C                   MOVE      '%'           PER3T
172500931005     C                   ELSE
172600970130     C                   MOVE      *BLANKS       PER3B
172700931005     C                   ENDIF
172800970130     C     PI88CS        IFNE      0
172900970130     C                   MOVE      '%'           PER3
173000970130     C                   MOVE      '%'           PER3T
173100970130     C                   ELSE
173200970130     C                   MOVE      *BLANKS       PER3
173300970130     C                   ENDIF
173400950622     C****CALCOLO % COLLI NON SPUNTATI CON PISTOLA 87 (EX 88)
173500950622     C     SKA123        IFNE      0
173600970130     C* DISK B
173700970130     C     PB87CS        DIV(H)    SKA123        $XX
173800970130     C     $XX           MULT      100           PC87CB            5 2
173900970130     C* ALTRI
174000950622     C     PI87CS        DIV(H)    SKA123        $XX
174100950622     C     $XX           MULT      100           PC87CS            5 2
174200970130     C* TOTALI
174300970130     C     PB87CS        ADD       PI87CS        P87CST            7 0
174400970203     C     P87CST        DIV(H)    SKA123        $XX
174500970130     C     $XX           MULT      100           PC87CT            5 2
174600950622     C                   ENDIF
174700950622     C*
174800970130     C                   CLEAR                   PER34T
174900970130     C     PB87CS        IFNE      0
175000970130     C                   MOVE      '%'           PER34B
175100970130     C                   MOVE      '%'           PER34T
175200950622     C                   ELSE
175300970130     C                   MOVE      *BLANKS       PER34B
175400950622     C                   ENDIF
175500970130     C     PI87CS        IFNE      0
175600970130     C                   MOVE      '%'           PER34
175700970130     C                   MOVE      '%'           PER34T
175800970130     C                   ELSE
175900970130     C                   MOVE      *BLANKS       PER34
176000970130     C                   ENDIF
176100930929     C**** **** **** **** **** **** **** **** ****
176200930929     C***CALCOLO TOTALI PESO
176300930929     C*TOTALE PESO FINO A 35 QUINTALI
176400930929     C     SKD(1)        ADD       SKD(2)        SKD123            8 1
176500930929     C                   ADD       SKD(3)        SKD123
176600090914     C                   ADD       SKD(4)        SKD123
176700930929     C*TOTALE PESO OLTRE  35 QUINTALI
176800930929     C     SKE(1)        ADD       SKE(2)        SKE123            8 1
176900930929     C                   ADD       SKE(3)        SKE123
177000090914     C                   ADD       SKE(4)        SKE123
177100930929     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
177200930929     C     SKF(1)        ADD       SKF(2)        SKF123            8 1
177300930929     C                   ADD       SKF(3)        SKF123
177400090914     C                   ADD       SKF(4)        SKF123
177500930929     C**** **** **** **** **** **** **** **** ****
177600930929     C***CALCOLO TOTALI VOLUMI
177700930929     C*TOTALE VOLUMI
177800941005     C     SKI(1)        ADD       SKI(2)        SKI123            9 4
177900930929     C                   ADD       SKI(3)        SKI123
178000090914     C                   ADD       SKI(4)        SKI123
178100930929     C**** **** **** **** **** **** **** **** ****
178200930929     C****CALCOLO % E TOTALI SPEDIZIONI
178300970130     C***CALCOLO % SPEDIZIONI FUORI AFFIDABILITA'
178400090914     C* SE GIORNO FESTIVO E NON SONO PARìITI COLLI CLEARO CAMPI DEL
178500970226     C* DA PARTIRE E FUORI AFFIDABILITA'
178600970226     C     WFES          IFEQ      'F'
178700970226     C     SKA(1)        ANDEQ     *ZEROS
178800970226     C     SKA(2)        ANDEQ     *ZEROS
178900970226     C     SKA(3)        ANDEQ     *ZEROS
179000090914     C     SKA(4)        ANDEQ     *ZEROS
179100970226     C                   CLEAR                   SKM(1)
179200970226     C                   CLEAR                   SKM(2)
179300970226     C                   CLEAR                   SKM(3)
179400090914     C                   CLEAR                   SKM(4)
179500970226     C                   CLEAR                   SKN(1)
179600970226     C                   CLEAR                   SKN(2)
179700970226     C                   CLEAR                   SKN(3)
179800090914     C                   CLEAR                   SKN(4)
179900970226     C                   CLEAR                   SKP(1)
180000970226     C                   CLEAR                   SKP(2)
180100970226     C                   CLEAR                   SKP(3)
180200090914     C                   CLEAR                   SKP(4)
180300970226     C                   END
180400930929     C*
180500930929     C***CALCOLO TOTALI SPEDIZIONI
180600970130     C*TOTALE SPEDIZIONI PARTITE
180700930929     C     SKL(1)        ADD       SKL(2)        SKL123            7 0
180800930929     C                   ADD       SKL(3)        SKL123
180900090914     C                   ADD       SKL(4)        SKL123
181000970130     C*TOTALE SPEDIZIONI DA PARTIRE
181100930929     C     SKM(1)        ADD       SKM(2)        SKM123            7 0
181200930929     C                   ADD       SKM(3)        SKM123
181300090914     C                   ADD       SKM(4)        SKM123
181400930929     C**** **** **** **** **** **** **** **** ****
181500930929     C****STAMPA FILIALE C/SERVIZI: DETTAGLIO E TOTALE.
181600931011     C     *INOF         IFEQ      *ON
181700941012     C  N36              EXCEPT    TESTA
181800020813     C                   Z-ADD     0             XD
181900931011     C                   ENDIF
182000931011     C*
182100941012     C  N36              EXCEPT    DETCS
182200930929     C**** **** **** **** **** **** **** **** ****
182300930929     C****SOMMA CAMPI TOTALI E PARZIALI FILIALE C/SERVIZI, IN CAMPI AREA
182400000616     C* SOLO SE STAMPO
182500000616     C     DS1RIS        IFNE      ' '
182600930930     C                   ADD       SKA123        AKA123            7 0
182700930929     C                   ADD       SKA(1)        AKA(1)
182800930929     C                   ADD       SKA(2)        AKA(2)
182900930929     C                   ADD       SKA(3)        AKA(3)
183000090914     C                   ADD       SKA(4)        AKA(4)
183100930929     C*
183200930929     C                   ADD       PI88CS        ARPI88            7 0
183300970130     C                   ADD       PB88CS        ARPB88            7 0
183400950622     C                   ADD       PI87CS        ARPI87            7 0
183500970130     C                   ADD       PB87CS        ARPB87            7 0
183600970130     C                   ADD       P88CST        ARP88T            7 0
183700970130     C                   ADD       P87CST        ARP87T            7 0
183800930929     C*
183900930930     C                   ADD       SKD123        AKD123            8 1
184000930929     C                   ADD       SKD(1)        AKD(1)
184100930929     C                   ADD       SKD(2)        AKD(2)
184200930929     C                   ADD       SKD(3)        AKD(3)
184300090914     C                   ADD       SKD(4)        AKD(4)
184400930929     C*
184500930930     C                   ADD       SKE123        AKE123            8 1
184600930929     C                   ADD       SKE(1)        AKE(1)
184700930929     C                   ADD       SKE(2)        AKE(2)
184800930929     C                   ADD       SKE(3)        AKE(3)
184900090914     C                   ADD       SKE(4)        AKE(4)
185000930929     C*
185100930930     C                   ADD       SKF123        AKF123            8 1
185200930929     C                   ADD       SKF(1)        AKF(1)
185300930929     C                   ADD       SKF(2)        AKF(2)
185400930929     C                   ADD       SKF(3)        AKF(3)
185500090914     C                   ADD       SKF(4)        AKF(4)
185600930929     C*
185700941005     C                   ADD       SKI123        AKI123            9 4
185800930929     C                   ADD       SKI(1)        AKI(1)
185900930929     C                   ADD       SKI(2)        AKI(2)
186000930929     C                   ADD       SKI(3)        AKI(3)
186100090914     C                   ADD       SKI(4)        AKI(4)
186200930929     C*
186300930930     C                   ADD       SKL123        AKL123            7 0
186400930930     C                   ADD       SKL(1)        AKL(1)
186500930929     C                   ADD       SKL(2)        AKL(2)
186600930929     C                   ADD       SKL(3)        AKL(3)
186700090914     C                   ADD       SKL(4)        AKL(4)
186800930929     C*
186900930930     C                   ADD       SKM123        AKM123            7 0
187000930929     C                   ADD       SKM(1)        AKM(1)
187100930929     C                   ADD       SKM(2)        AKM(2)
187200930929     C                   ADD       SKM(3)        AKM(3)
187300090914     C                   ADD       SKM(4)        AKM(4)
187400930929     C*
187500930929     C                   ADD       SKN(1)        AKN(1)
187600930929     C                   ADD       SKN(2)        AKN(2)
187700930929     C                   ADD       SKN(3)        AKN(3)
187800090914     C                   ADD       SKN(4)        AKN(4)
187900000616     C                   ENDIF
188000930929     C*
188100930929     C**** **** **** **** **** **** **** **** ****
188200930929     C****PULIZIA CAMPI FILIALE C/SERVIZI (DI TOTALE E DI DETTAGLIO)
188300930929     C*CAMPI DI TOTALE
188400930929     C                   Z-ADD     0             SKA123
188500930929     C                   Z-ADD     0             PI88CS
188600970130     C                   Z-ADD     0             PB88CS
188700930929     C                   Z-ADD     0             PC88CS
188800970130     C                   Z-ADD     0             PC88CB
188900970130     C                   Z-ADD     0             P88CST
189000970130     C                   Z-ADD     0             PC88CT
189100950622     C                   Z-ADD     0             PI87CS
189200970130     C                   Z-ADD     0             PB87CS
189300950622     C                   Z-ADD     0             PC87CS
189400970130     C                   Z-ADD     0             PC87CB
189500970130     C                   Z-ADD     0             P87CST
189600970130     C                   Z-ADD     0             PC87CT
189700930929     C                   Z-ADD     0             SKD123
189800930929     C                   Z-ADD     0             SKE123
189900930929     C                   Z-ADD     0             SKF123
190000930929     C                   Z-ADD     0             SKI123
190100930929     C                   Z-ADD     0             SKL123
190200930929     C                   Z-ADD     0             SKM123
190300930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
190400930929     C*PER LA STAMPA).
190500930929     C*
190600931005     C                   MOVEA     *ZEROS        SKA(1)
190700930929     C*
190800931005     C                   MOVEA     *ZEROS        SKD(1)
190900930929     C*
191000931005     C                   MOVEA     *ZEROS        SKE(1)
191100930929     C*
191200931005     C                   MOVEA     *ZEROS        SKF(1)
191300930929     C*
191400931005     C                   MOVEA     *ZEROS        SKI(1)
191500930929     C*
191600931005     C                   MOVEA     *ZEROS        SKL(1)
191700930929     C*
191800931005     C                   MOVEA     *ZEROS        SKM(1)
191900930929     C*
192000931005     C                   MOVEA     *ZEROS        SKN(1)
192100970206     C                   MOVEA     *ZEROS        SKO(1)
192200970206     C                   MOVEA     *ZEROS        SKP(1)
192300930929     C*
192400930929     C                   ENDSR
192500930929     C****************************************************************
192600930929     C* SUBROUTINE TOTARE:
192700930929     C**CALCOLO %, CALCOLO TOTALI AREA, STAMPA DETTAGLIO AREA,
192800930929     C*****************************************************************
192900930929     C     TOTARE        BEGSR
193000970130     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA PISTOLA 88
193100930930     C     AKA123        IFNE      0
193200970130     C* DISK B
193300970130     C     ARPB88        DIV(H)    AKA123        $XX
193400970130     C     $XX           MULT      100           APC88B            5 2
193500970130     C* ALTRI
193600970130     C     ARPI88        DIV(H)    AKA123        $XX
193700970130     C     $XX           MULT      100           ARPC88            5 2
193800970130     C* TOTALE
193900970130     C     ARP88T        DIV(H)    AKA123        $XX
194000970130     C     $XX           MULT      100           APC88T            5 2
194100930930     C                   ENDIF
194200931005     C*
194300970130     C                   CLEAR                   PER3T
194400970130     C     ARPB88        IFNE      0
194500970130     C                   MOVE      '%'           PER3B
194600970130     C                   MOVE      '%'           PER3T
194700931005     C                   ELSE
194800970130     C                   MOVE      *BLANKS       PER3B
194900931005     C                   ENDIF
195000970130     C     ARPI88        IFNE      0
195100970130     C                   MOVE      '%'           PER3
195200970130     C                   MOVE      '%'           PER3T
195300970130     C                   ELSE
195400970130     C                   MOVE      *BLANKS       PER3
195500970130     C                   ENDIF
195600950622     C****CALCOLO % COLLI SPUNTATI CON PISTOLA 87 (EX 88)
195700950622     C     AKA123        IFNE      0
195800970130     C* DISK B
195900970130     C     ARPB87        DIV(H)    AKA123        $XX
196000970130     C     $XX           MULT      100           APC87B            5 2
196100970130     C* ALTRI
196200950622     C     ARPI87        DIV(H)    AKA123        $XX
196300950622     C     $XX           MULT      100           ARPC87            5 2
196400970130     C* TOTALE
196500970130     C     ARP87T        DIV(H)    AKA123        $XX
196600970130     C     $XX           MULT      100           APC87T            5 2
196700950622     C                   ENDIF
196800950622     C*
196900970130     C                   CLEAR                   PER34T
197000970130     C     ARPB87        IFNE      0
197100970130     C                   MOVE      '%'           PER34B
197200970130     C                   MOVE      '%'           PER34T
197300950622     C                   ELSE
197400970130     C                   MOVE      *BLANKS       PER34B
197500950622     C                   ENDIF
197600970130     C     ARPI87        IFNE      0
197700970130     C                   MOVE      '%'           PER34
197800970130     C                   MOVE      '%'           PER34T
197900970130     C                   ELSE
198000970130     C                   MOVE      *BLANKS       PER34
198100970130     C                   ENDIF
198200930929     C**** **** **** **** **** **** **** **** ****
198300930929     C****STAMPA AREA :DETTAGLIO E TOTALE.
198400020813     C     XD            IFLE      7
198500941012     C  N36              EXCEPT    DETARE
198600931011     C                   ELSE
198700941012     C  N36              EXCEPT    TESTA
198800941012     C  N36              EXCEPT    DETARE
198900931011     C                   ENDIF
199000930929     C**** **** **** **** **** **** **** **** ****
199100931007     C****SOMMA CAMPI TOTALI AREA, IN CAMPI TOT. GENERALE
199200000616     C* SOLO SE  STAMPO
199300000616     C     DS1RIS        IFNE      ' '
199400931006     C*
199500931008     C                   ADD       AKA123        TKA123            7 0
199600931006     C                   ADD       AKA(1)        TKA(1)
199700931006     C                   ADD       AKA(2)        TKA(2)
199800931006     C                   ADD       AKA(3)        TKA(3)
199900090914     C                   ADD       AKA(4)        TKA(4)
200000931006     C*
200100970130     C                   ADD       ARP88T        TGP88T            7 0
200200970130     C                   ADD       ARP87T        TGP87T            7 0
200300931006     C*
200400931008     C                   ADD       AKD123        TKD123            8 1
200500931006     C                   ADD       AKD(1)        TKD(1)
200600931006     C                   ADD       AKD(2)        TKD(2)
200700931006     C                   ADD       AKD(3)        TKD(3)
200800090914     C                   ADD       AKD(4)        TKD(4)
200900931006     C*
201000931008     C                   ADD       AKE123        TKE123            8 1
201100931006     C                   ADD       AKE(1)        TKE(1)
201200931006     C                   ADD       AKE(2)        TKE(2)
201300931006     C                   ADD       AKE(3)        TKE(3)
201400090914     C                   ADD       AKE(4)        TKE(4)
201500931006     C*
201600931008     C                   ADD       AKF123        TKF123            8 1
201700931006     C                   ADD       AKF(1)        TKF(1)
201800931006     C                   ADD       AKF(2)        TKF(2)
201900931006     C                   ADD       AKF(3)        TKF(3)
202000090914     C                   ADD       AKF(4)        TKF(4)
202100931006     C*
202200931007     C                   ADD       ARPETS        TGPETS            9 1
202300931006     C*
202400941005     C                   ADD       AKI123        TKI123            9 4
202500931006     C                   ADD       AKI(1)        TKI(1)
202600931006     C                   ADD       AKI(2)        TKI(2)
202700931006     C                   ADD       AKI(3)        TKI(3)
202800090914     C                   ADD       AKI(4)        TKI(4)
202900931006     C*
203000931008     C                   ADD       AKL123        TKL123            7 0
203100931006     C                   ADD       AKL(1)        TKL(1)
203200931006     C                   ADD       AKL(2)        TKL(2)
203300931006     C                   ADD       AKL(3)        TKL(3)
203400090914     C                   ADD       AKL(4)        TKL(4)
203500000616     C                   ENDIF
203600931006     C*
203700930930     C**** **** **** **** **** **** **** **** ****
203800930930     C****PULIZIA CAMPI AREA    (DI TOTALE E DI DETTAGLIO)
203900930929     C*CAMPI DI TOTALE
204000930930     C                   Z-ADD     0             AKA123
204100930930     C                   Z-ADD     0             ARPI88
204200970130     C                   Z-ADD     0             ARPB88
204300970130     C                   Z-ADD     0             APC88B
204400970130     C                   Z-ADD     0             ARPC88
204500970130     C                   Z-ADD     0             ARP88T
204600970130     C                   Z-ADD     0             APC88T
204700950622     C                   Z-ADD     0             ARPI87
204800970130     C                   Z-ADD     0             ARPB87
204900950622     C                   Z-ADD     0             ARPC87
205000970130     C                   Z-ADD     0             APC87B
205100970130     C                   Z-ADD     0             ARP87T
205200970130     C                   Z-ADD     0             APC87T
205300930930     C                   Z-ADD     0             AKD123
205400930930     C                   Z-ADD     0             AKE123
205500930930     C                   Z-ADD     0             AKF123
205600930930     C                   Z-ADD     0             ARPETS
205700930930     C                   Z-ADD     0             AKI123
205800930930     C                   Z-ADD     0             AKL123
205900930930     C                   Z-ADD     0             AKM123
206000930929     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
206100930929     C*PER LA STAMPA).
206200931005     C                   MOVEA     *ZEROS        AKA(1)
206300930929     C*
206400931005     C                   MOVEA     *ZEROS        AKD(1)
206500930929     C*
206600931005     C                   MOVEA     *ZEROS        AKE(1)
206700930929     C*
206800931005     C                   MOVEA     *ZEROS        AKF(1)
206900930929     C*
207000931005     C                   MOVEA     *ZEROS        AKI(1)
207100930929     C*
207200931005     C                   MOVEA     *ZEROS        AKL(1)
207300930929     C*
207400931005     C                   MOVEA     *ZEROS        AKM(1)
207500930929     C*
207600931005     C                   MOVEA     *ZEROS        AKN(1)
207700930929     C*
207800930929     C                   ENDSR
207900930929     C****************************************************************
208000931006     C* SUBROUTINE ELATRA: ELABORAZIONE TRANSITI
208100931006     C*****************************************************************
208200931006     C     ELATRA        BEGSR
208300950111     C*
208400931008     C                   Z-ADD     0             FLDTXT
208500950111     C*
208600950111     C* ?    C O L L I    T R A N S I T O    ?
208700950329     C*
208800931007     C*POSIZ.PER'DATA DA'(DATA USCITA DAL TRANSITO).
208900020823     C     KDTA          SETLL     FNBTT38L
209000020823     C     KDTA          READE     FNBTT38L                               31
209100931006     C* CICLO LETTURA COLLI
209200970130    1C     *IN31         DOWEQ     *OFF
209300931007     C*
209400931013     C* ENTRA SOLO AL CAMBIO BOLLA.
209500970130    2C     DSTXT1        IFNE      FLDTXT
209600950111     C                   Z-ADD     DSTXT1        FLDTXT
209700020823     C     KBTP11        CHAIN     FNBTP11L                           32
209800931130      *
209900931130      * SE TROVA LA BOLLA (N32), TESTA SE E' DI RECUPERO
210000970130    3C     *IN32         IFEQ      *OFF
210100950111     C*
210200950111     C                   Z-ADD     1             WA
210300950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
210400950111     C   32              MOVEL     D3A(WA)       DS3A
210500950111     C  N32              CLEAR                   DS3A
210600931130      *
210700950111     C* REPERIM.DATI BOLLA, (CORR.,ESPR.,RAPID,NON DIFFERENZIANDO I C/SERV.)
210800931007     C* QUINDI SOLO PER BOLLE-TRANSITO.
210900931007     C                   EXSR      DTBOT
211000950111     C* REPERIMENTO DATI BOLLA, PER CALCOLO TRANSITI
211100950111     C                   EXSR      DATBOT
211200931013     C*ALTRIMENTI SE NON LA TROVA, LA TRASFORMA IN RECUPERO PER FARLA SCARTARE
211300970130   X3C                   ELSE
211400931006     C                   MOVEL     'R'           §3ARBL
211500970130    3C                   ENDIF
211600970130    2C                   ENDIF
211700931006     C*
211800931013     C* SE NON E' UN RECUPERO, VADO AL CALCOLO.
211900970130    2C     §3ARBL        IFNE      'R'
212000931013     C* CALCOLI-TRANSITI
212100931007     C                   EXSR      CALCTR
212200970130    2C                   ENDIF
212300931006     C*
212400020823     C     KDTA          READE     FNBTT38L                               31
212500970130    1C                   ENDDO
212600970505     C* AGGIUNGO I COLLI IN DISGUIDO
212700970505     C                   ADD       WDIS88        TRPI88
212800000616     C                   CLEAR                   WDIS88
212900931006     C***
213000950111     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
213100950111     C* ?    S P E D I Z I O N I    T R A N S I T O    ?
213200931007     C*
213300950111     C* POSIZIONAMENTO PER DATA INIZIO PERIODO
213400020823     C     KDTA          SETLL     FNBTP15L
213500020823     C     KDTA          READE     FNBTP15L                               31
213600931013     C*
213700970130    1C     *IN31         DOWEQ     *OFF
213800931007     C*
213900931130      * TEST SE E' RECUPERO
214000950111     C                   Z-ADD     1             WA
214100950111     C     BTPCBO        LOOKUP    E3A(WA)                                32
214200950111     C   32              MOVEL     D3A(WA)       DS3A
214300950111     C  N32              CLEAR                   DS3A
214400931006     C*
214500931006     C* SE TUTTO OK, ESEGUE LA SUBROUTINE DI CALCOLO SPEDIZIONI.
214600970130    2C     §3ARBL        IFNE      'R'
214700931007     C                   EXSR      CSPETR
214800970130    2C                   ENDIF
214900931006     C*
215000950111     C*
215100931006     C*LETTURA DENTRO AL CICLO
215200020823     C     KDTA          READE     FNBTP15L                               31
215300970130    1C                   ENDDO
215400931006     C*
215500931006     C                   ENDSR
215600931007     C*****************************************************************
215700931007     C* SUBROUTINE DTBOT: NON DIFFERENZIA TRA FILIALE E C/SERVIZI
215800931007     C*****************************************************************
215900931007     C     DTBOT         BEGSR
216000950111     C*
216100931007     C*TEST SE E' C/SERVIZI O FILIALE
216200931007     C*MI SERVE SOLO PER SAPERE SE ADDIZIONARE O MENO IL PESO TASSABILE.
216300931007     C     §3ARBL        IFEQ      'C'
216400931007     C                   SETON                                        04
216500931007     C                   ELSE
216600931007     C                   SETOFF                                       04
216700931007     C                   ENDIF
216800931007     C*
216900931007     C*FISSO GLI INDICI DI SCHIERA....
217000931007     C*
217100090914     C* SE TIPO SPEDIZIONE E' h 10 30
217200090914     c                   select
217300090914     C     BTPTSP        wheneq    'H'
217400090914     C                   Z-ADD     4             X
217500090914     c
217600090914     C* SE TIPO SPEDIZIONE E' priority
217700090914     C     BTPTSP        wheneq    'E'
217800931007     C                   Z-ADD     2             X
217900931007     C*
218000950111     C* SE TIPO SPEDIZIONE E' DISTRIBUZIONE
218100090914     C     BTPTSP        wheneq    'D'
218200931007     C                   Z-ADD     3             X
218300090914     C                   other
218400950111     C                   Z-ADD     1             X
218500090914     C                   ENDSL
218600931007     C*
218700931007     C                   ENDSR
218800950111     C************************************************************
218900950111     C* SUBROUTINE DATBOT: REPERIMENTO DATI BOLLA PER CALCOLI (TRANSITI)
219000950111     C*****************************************************************
219100950111     C     DATBOT        BEGSR
219200021213     ***C*
219300021213     ***C*CALCOLO PESO DI UN COLLO, DA USARE PER SOMME PESO
219400021213     ***C     BTPPKB        DIV(H)    BTPNCL        PESCOL            8 1
219500021213     *** *
219600021213     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
219700021213     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
219800021213     *** * BTPVLC=Volume rilevato)
219900050111     ***C     BTPNCR        IFEQ      BTPNCL
220000021213     ***C                   Z-ADD     BTPVLC        BTPVLB
220100021213     ***C                   ELSE
220200021213     ***C     BTPVLC        IFGE      BTPVLB
220300021213     ***C                   Z-ADD     BTPVLC        BTPVLB
220400021213     ***C                   ENDIF
220500021213     ***C                   ENDIF
220600021213     ***C*
220700021213     ***C*CALCOLO VOLUME DI UN COLLO, (VOLUM) DA USARE PER SOMME
220800021213     ***C     BTPVLB        DIV(H)    BTPNCL        VOLUM            12 4
220900021220      *
221000021220      * Verifico i dati misurati con la macchina smistacolli
221100021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
221200021220      * e calcolo il VOLUME unitario medio per collo da usare se non
221300021220      * tutti i colli sono stati volumati dalla smistacolli
221400021220      *
221500030213      **** Verifico se sono stati volumati tutti i colli
221600030213     c***                clear                   Wsivol
221700021220     c                   clear                   Wvolu
221800050111     c                   IF        BTPncr = BTPncl
221900021220     c                   z-add     BTPvlc        Wvolu
222000030213     c***                eval      Wsivol =  'S'
222100021220   x1c                   else
222200021220if  2c                   if        BTPvlc >= BTPvlb
222300021220     c                   z-add     BTPvlc        Wvolu
222400021220   x2c                   else
222500021220     c                   z-add     BTPvlb        Wvolu
222600021220e   2c                   endif
222700021220e   1c                   endif
222800021220      *
222900021220      * Calcolo il volume medio
223000021220     c     Wvolu         div(H)    BTPncl        wVOLUM
223100021220      *
223200021220      * Verifico i dati misurati con la macchina smistacolli
223300021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
223400021220      * e calcolo il PESO   unitario medio per collo da usare se non
223500021220      * tutti i colli sono stati pesati   dalla smistacolli
223600021220      *
223700030213      **** Verifico se sono stati pesati   tutti i colli
223800030213     c***                clear                   Wsipes
223900021220     c                   clear                   Wpeso
224000050111     c                   If        BTPncp = BTPncl
224100021220     c                   z-add     BTPpkc        Wpeso
224200030213     c***                eval      Wsipes =  'S'
224300021220   x1c                   else
224400021220if  2c                   if        BTPpkc >= BTPpkb
224500021220     c                   z-add     BTPpkc        Wpeso
224600021220   x2c                   else
224700021220     c                   z-add     BTPpkb        Wpeso
224800021220e   2c                   endif
224900021220e   1c                   endif
225000021220      *
225100021220      * Calcolo il peso medio
225200021220     c     Wpeso         div(H)    BTPncl        wPESCOL
225300950111     C*
225400950111     C*VEDO SE PESO SPEDIZIONE (BOLLA) E' FINO A 35 Q (3500 KG), OD OLTRE.
225500021213     ***C     BTPPKB        IFLE      3500
225600021213     C     Wpeso         IFLE      3500
225700950111     C                   SETON                                        06
225800950111     C                   ELSE
225900950111     C                   SETOFF                                       06
226000950111     C                   ENDIF
226100950111     C*
226200950111     C*CALCOLO PESO TASSABILE DELLA SPEDIZIONE (PESTAX)
226300021213     ***C                   Z-ADD     BTPPKB        PESTAX            8 0
226400021213     C                   z-add     Wpeso         PESTAX
226500950111     C*
226600950111     C*MOLTIPLICA IL VOLUME PER CAMPO FISSO AZIENDALE DEL RAPPORTO PESO/VOLUME
226700021213     ***C     BTPVLB        MULT      §QTKPM        COMPKT
226800021213     C     Wvolu         MULT      §QTKPM        COMPKT
226900950111     C*PER CALC.VOLUME TASSABILE, DEVE USARE IL MAGGIOR PESO TRA IL REALE E
227000950111     C*QUELLO CALCOLATO CON RAPPORTO FISSO AZIENDALE
227100950111     C     COMPKT        IFGT      PESTAX
227200950111     C                   Z-ADD     COMPKT        PESTAX
227300950111     C                   END
227400950111     C* L'ARROTONDAMENTO (E' PRESO DALLA TABELLA 3I §3IARR)
227500950111     C                   Z-ADD     0             COMPKT            8 0
227600950111     C     PESTAX        DIV       §3IARR        COMPKT
227700950111     C                   MVR                     RESTO             3 0
227800950111     C     RESTO         IFGT      0
227900950111     C     COMPKT        MULT      §3IARR        PESTAX
228000950111     C                   ADD       §3IARR        PESTAX
228100950111     C                   ENDIF
228200950111     C*TROVO IL PESO TASSABILE DI UN COLLO (PESTAS),DA USARE PER SOMME
228300021213     C     PESTAX        DIV(H)    BTPNCL        PESTAS
228400950111     C*
228500950111     C                   ENDSR
228600931007     C****************************************************************
228700931007     C* SUBROUTINE CALCTR:
228800931007     C* SOMMA VALORI COLLO, IN TRANSITI.
228900931007     C* COLLI;COLLI NON SPUNT;PESO;PESO TASSABILE;VOLUME.
229000931007     C*****************************************************************
229100931007     C     CALCTR        BEGSR
229200950111     C*
229300931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
229400931007     C******COLLI
229500931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
229600931007     C                   ADD       1             XKA(X)
229700931007     C*
229800931007     C******COLLI NON SPUNTATI IN PARTENZA: 88.
229900950111     C     BTTPUT        IFEQ      88
230000950331     C     BTTPUT        OREQ      89
230100970430     C     BTTPUT        OREQ      98
230200970430     C     BTTPUT        OREQ      96
230300941012     C  N04              ADD       1             TRPI88            7 0
230400931007     C                   ENDIF
230500950622     C******COLLI PISTOLA 87 (EX 88 E 89)
230600950622     C     BTTPUT        IFEQ      87
230700950622     C  N04              ADD       1             TRPI87            7 0
230800950622     C                   ENDIF
230900931007     C*
231000931007     C******PESO
231100931007     C*PESCOL= PESO DI UN COLLO, DA USARE PER SOMME COLLO
231200030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
231300030213      * Peso medio
231400030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
231500030213if  1c***                if        Wsipes = 'S'
231600030213     c***                z-add(H)  BTTpuc        PESCOL
231700030213   x1c***                else
231800021220     c                   z-add     Wpescol       PESCOL
231900030213e   1c***                endif
232000950403     C*SE SPEDIZIONE (BOLLA) FINO A 35 Q.
232100931007     C   06              ADD       PESCOL        XKD(X)
232200931007     C*SE SPEDIZIONE (BOLLA) OLTRE  35 Q.
232300931007     C  N06              ADD       PESCOL        XKE(X)
232400931007     C*IN TUTTI I CASI SOMMO IN SCHIERA DEI TOTALI
232500931007     C                   ADD       PESCOL        XKF(X)
232600931007     C*
232700931007     C*
232800931007     C******PESO TASSABILE
232900931007     C  N04
233000931007     CAN 06              ADD       PESTAS        TRPETS            9 1
233100931007     C*
233200931007     C*
233300931007     C******VOLUME
233400021213     ***C     BTTVUC        IFGT      0
233500021213     ***C                   Z-ADD(H)  BTTVUC        WVOLUM
233600021213     ***C                   ELSE
233700021213     ***C                   Z-ADD(H)  VOLUM         WVOLUM
233800021213     ***C                   ENDIF
233900021220     ***C*
234000021220     ***C                   ADD       WVOLUM        XKI(X)
234100030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
234200030213      * Volume medio
234300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
234400030213if  1c***                if        Wsivol = 'S'
234500030213     c***                z-add     BTTvuc        Volum
234600030213   x1c***                else
234700021220     c                   z-add     Wvolum        Volum
234800030213e   1c***                endif
234900021220     c                   add       VOLUM         XKI(X)
235000931007     C*
235100931007     C                   ENDSR
235200930929     C****************************************************************
235300931007     C* SUBROUTINE CSPETR:  CALCOLO SPEDIZIONI -TRANSITI
235400931007     C* SOMMA VALORI COLLO, IN FILIALE O IN C/SERVIZI:
235500931007     C*****************************************************************
235600931007     C     CSPETR        BEGSR
235700931007     C*
235800931007     C*FISSO GLI INDICI DI SCHIERA....
235900090914     c                   exsr      DTBOT
236000931007     C*
236100931007     C*
236200931007     C******SPEDIZIONI
236300931007     C*(PER I TRANSITI NON C'E' LA PARTIZIONE PER % PERIODO E PERIODO PRECED).
236400931007     C* SOMMO SOLO IN SCHIERA DEL TOTALE
236500931007     C                   ADD       1             XKL(X)
236600931007     C*
236700931007     C                   ENDSR
236800931007     C****************************************************************
236900931007     C* SUBROUTINE TOTTRA:
237000931007     C**CALCOLO TOTALI TRANSITI STAMPA DETTAGLIO TRANSITI
237100931007     C*****************************************************************
237200931007     C     TOTTRA        BEGSR
237300950111     C*
237400931007     C**** **** **** **** **** **** **** **** ****
237500970218     C* AGGIUNGO I COLLI IN DISGUIDO AL TOTALE COLLI CORRIERE
237600970218     C                   ADD       WDIS          XKA(1)
237700000616     C                   CLEAR                   WDIS
237800931007     C****CALCOLO TOTALE COLLI:
237900931007     C     XKA(1)        ADD       XKA(2)        XKA123            7 0
238000931008     C                   ADD       XKA(3)        XKA123
238100090914     C                   ADD       XKA(4)        XKA123
238200950331     C*
238300931007     C**** **** **** **** **** **** **** **** ****
238400931007     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
238500931007     C     XKA123        IFNE      0
238600931007     C     TRPI88        DIV(H)    XKA123        $XX
238700931007     C     $XX           MULT      100           TRPC88            5 2
238800931007     C                   ENDIF
238900931007     C*
239000931008     C     TRPI88        IFNE      0
239100931007     C                   MOVE      '%'           PER3              1
239200931007     C                   ELSE
239300931007     C                   MOVE      *BLANKS       PER3
239400931007     C                   ENDIF
239500950622     C****CALCOLO % COLLI PISTOLA 87 (EX 88 E 89)
239600950622     C     XKA123        IFNE      0
239700950622     C     TRPI87        DIV(H)    XKA123        $XX
239800950622     C     $XX           MULT      100           TRPC87            5 2
239900950622     C                   ENDIF
240000950622     C*
240100950622     C     TRPI87        IFNE      0
240200950622     C                   MOVE      '%'           PER34             1
240300950622     C                   ELSE
240400950622     C                   MOVE      *BLANKS       PER34
240500950622     C                   ENDIF
240600931007     C**** **** **** **** **** **** **** **** ****
240700931007     C****CALCOLO TOTALI PESO:
240800931007     C*
240900931007     C*TOTALE PESO FINO A 35 QUINTALI
241000931007     C     XKD(1)        ADD       XKD(2)        XKD123            8 1
241100931007     C                   ADD       XKD(3)        XKD123
241200090914     C                   ADD       XKD(4)        XKD123
241300931007     C*TOTALE PESO OLTRE  35 QUINTALI
241400931007     C     XKE(1)        ADD       XKE(2)        XKE123            8 1
241500931007     C                   ADD       XKE(3)        XKE123
241600090914     C                   ADD       XKE(4)        XKE123
241700931007     C*TOTALE PESO (FINO 35 Q. + OLTRE 35 Q.)
241800931007     C     XKF(1)        ADD       XKF(2)        XKF123            8 1
241900931007     C                   ADD       XKF(3)        XKF123
242000090914     C                   ADD       XKF(4)        XKF123
242100931007     C**** **** **** **** **** **** **** **** ****
242200931007     C****CALCOLO TOTALE VOLUME
242300941005     C     XKI(1)        ADD       XKI(2)        XKI123            9 4
242400931007     C                   ADD       XKI(3)        XKI123
242500090914     C                   ADD       XKI(4)        XKI123
242600931007     C**** **** **** **** **** **** **** **** ****
242700931007     C****CALCOLO TOTALE SPEDIZIONI
242800931007     C     XKL(1)        ADD       XKL(2)        XKL123            7 0
242900931007     C                   ADD       XKL(3)        XKL123
243000090914     C                   ADD       XKL(4)        XKL123
243100931007     C**** **** **** **** **** **** **** **** ****
243200931007     C****STAMPA TRANSITI :DETTAGLIO E TOTALE.
243300941012     C  N36              EXCEPT    DETTRA
243400950210     C     DS1RIS        IFNE      'R'
243500941012     C                   EXSR      WRITRA
243600941012     C                   ENDIF
243700931007     C**** **** **** **** **** **** **** **** ****
243800931007     C****SOMMA CAMPI TOTALI TRANSITI IN CAMPI TOTALI TOT.GEN.
243900000616     C**  SOLO SE NON STAMPO
244000000616     C     DS1RIS        IFNE      ' '
244100931007     C                   ADD       XKA123        TKA123
244200931007     C                   ADD       XKA(1)        TKA(1)
244300931007     C                   ADD       XKA(2)        TKA(2)
244400931007     C                   ADD       XKA(3)        TKA(3)
244500090914     C                   ADD       XKA(4)        TKA(4)
244600931007     C*
244700970130     C                   ADD       TRPI88        TGP88T            7 0
244800970130     C                   ADD       TRPI87        TGP87T            7 0
244900931007     C*
245000931007     C                   ADD       XKD123        TKD123
245100931007     C                   ADD       XKD(1)        TKD(1)
245200931007     C                   ADD       XKD(2)        TKD(2)
245300931007     C                   ADD       XKD(3)        TKD(3)
245400090914     C                   ADD       XKD(4)        TKD(4)
245500931007     C*
245600931007     C                   ADD       XKE123        TKE123
245700931007     C                   ADD       XKE(1)        TKE(1)
245800931007     C                   ADD       XKE(2)        TKE(2)
245900931007     C                   ADD       XKE(3)        TKE(3)
246000090914     C                   ADD       XKE(4)        TKE(4)
246100931007     C*
246200931007     C                   ADD       XKF123        TKF123
246300931007     C                   ADD       XKF(1)        TKF(1)
246400931007     C                   ADD       XKF(2)        TKF(2)
246500931007     C                   ADD       XKF(3)        TKF(3)
246600090914     C                   ADD       XKF(4)        TKF(4)
246700931007     C*
246800931007     C                   ADD       TRPETS        TGPETS            9 1
246900931007     C*
247000931007     C                   ADD       XKI123        TKI123
247100931007     C                   ADD       XKI(1)        TKI(1)
247200931007     C                   ADD       XKI(2)        TKI(2)
247300931007     C                   ADD       XKI(3)        TKI(3)
247400090914     C                   ADD       XKI(4)        TKI(4)
247500931007     C*
247600931007     C                   ADD       XKL123        TKL123
247700931007     C                   ADD       XKL(1)        TKL(1)
247800931007     C                   ADD       XKL(2)        TKL(2)
247900931007     C                   ADD       XKL(3)        TKL(3)
248000090914     C                   ADD       XKL(4)        TKL(4)
248100000616     C                   ENDIF
248200931007     C*
248300931007     C**** **** **** **** **** **** **** **** ****
248400931008     C****PULIZIA CAMPI TRANSITO (DI TOTALE E DI DETTAGLIO)
248500931007     C*CAMPI DI TOTALE
248600931007     C                   Z-ADD     0             XKA123
248700931007     C                   Z-ADD     0             TRPI88
248800931007     C                   Z-ADD     0             TRPC88
248900950622     C                   Z-ADD     0             TRPI87
249000950622     C                   Z-ADD     0             TRPC87
249100931007     C                   Z-ADD     0             XKD123
249200931007     C                   Z-ADD     0             XKE123
249300931007     C                   Z-ADD     0             XKF123
249400931007     C                   Z-ADD     0             TRPETS
249500931007     C                   Z-ADD     0             XKI123
249600931007     C                   Z-ADD     0             XKL123
249700931007     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
249800931007     C*PER LA STAMPA).
249900931007     C                   MOVEA     *ZEROS        XKA(1)
250000931007     C*
250100931007     C                   MOVEA     *ZEROS        XKD(1)
250200931007     C*
250300931007     C                   MOVEA     *ZEROS        XKE(1)
250400931007     C*
250500931007     C                   MOVEA     *ZEROS        XKF(1)
250600931007     C*
250700931007     C                   MOVEA     *ZEROS        XKI(1)
250800931007     C*
250900931007     C                   MOVEA     *ZEROS        XKL(1)
251000931007     C*
251100931007     C                   ENDSR
251200931007     C****************************************************************
251300931008     C* SUBROUTINE TOTGEN:
251400931008     C**CALCOLO TOTALI TOT.GEN. STAMPA DETTAGLIO TOT.GEN.
251500931008     C*****************************************************************
251600931008     C     TOTGEN        BEGSR
251700950111     C*
251800931008     C**** **** **** **** **** **** **** **** ****
251900931008     C****CALCOLO % COLLI NON SPUNTATI IN PARTENZA
252000931008     C     TKA123        IFNE      0
252100970130     C* TOTALE
252200970131     C     TGP88T        DIV(H)    TKA123        $XX
252300970131     C     $XX           MULT      100           TGC88T            5 2
252400931008     C                   ENDIF
252500931008     C*
252600970131     C     TGP88T        IFNE      0
252700970131     C                   MOVE      '%'           PER3              1
252800970131     C                   ELSE
252900970131     C                   MOVE      *BLANKS       PER3
253000970131     C                   ENDIF
253100950622     C****CALCOLO % COLLI CON PISTOLA 87 (EX 88 E 89)
253200950622     C     TKA123        IFNE      0
253300970130     C* TOTALE
253400970131     C     TGP87T        DIV(H)    TKA123        $XX
253500970131     C     $XX           MULT      100           TGC87T            5 2
253600950622     C                   ENDIF
253700950622     C*
253800970131     C     TGP87T        IFNE      0
253900950622     C                   MOVE      '%'           PER34             1
254000950622     C                   ELSE
254100950622     C                   MOVE      *BLANKS       PER34
254200950622     C                   ENDIF
254300931008     C**** **** **** **** **** **** **** **** ****
254400931008     C****STAMPA TOT.GEN. :DETTAGLIO E TOTALE.
254500941012     C  N36              EXCEPT    DETTOT
254600931008     C**** **** **** **** **** **** **** **** ****
254700931008     C****PULIZIA CAMPI TOT.GEN.(DI TOTALE E DI DETTAGLIO)
254800931008     C*CAMPI DI TOTALE
254900931008     C                   Z-ADD     0             TKA123
255000970131     C                   Z-ADD     0             TGP88T
255100970131     C                   Z-ADD     0             TGP87T
255200970131     C                   Z-ADD     0             TGC88T
255300970131     C                   Z-ADD     0             TGC87T
255400931008     C                   Z-ADD     0             TKD123
255500931008     C                   Z-ADD     0             TKE123
255600931008     C                   Z-ADD     0             TKF123
255700931008     C                   Z-ADD     0             TGPETS
255800931008     C                   Z-ADD     0             TKI123
255900931008     C                   Z-ADD     0             TKL123
256000931008     C*CAMPI DI DETTAGLIO  (SIA USATI SOLO PER IL CALCOLO, CHE USATI ANCHE
256100931008     C*PER LA STAMPA).
256200931008     C                   MOVEA     *ZEROS        TKA(1)
256300931008     C                   MOVEA     *ZEROS        TKD(1)
256400931008     C                   MOVEA     *ZEROS        TKE(1)
256500931008     C                   MOVEA     *ZEROS        TKF(1)
256600931008     C                   MOVEA     *ZEROS        TKI(1)
256700931008     C                   MOVEA     *ZEROS        TKL(1)
256800940126      *
256900941012      * Verifico se vi sono fogli viaggio aperti
257000941012     C                   EXSR      FOGVIA
257100931130      *
257200931008     C                   ENDSR
257300941012      ****************************************************************
257400941012      * Scrivo in archivio il riepilogo per filiale (totali e c/serv.)
257500941012      ****************************************************************
257600941012     C     WRIFIL        BEGSR
257700941012      *
257800941012      * ---------------------- TOTALI DI FILIALE -------------------------------
257900941012      *
258000941012     C                   Z-ADD     SIMFEL        SPAFLE
258100941012     C                   Z-ADD     COMFIL        SPALNP
258200950210     C                   Z-ADD     DS1ADA        SPADRE
258300941012     C                   MOVE      *BLANK        SPATRE
258400941012      * ---- COLLI ----
258500090914     C                   Z-ADD     SKA(6)        SPACPC
258600090914     C                   Z-ADD     SKA(7)        SPACPE
258700090914     C                   Z-ADD     SKA(8)        SPACPD
258800090914     C                   Z-ADD     SKA(9)        SPACPh
258900970129     C                   Z-ADD     PB88F         SPA88B
259000970129     C                   Z-ADD     PI88          SPA88A
259100970129     C                   Z-ADD     PB87F         SPA87B
259200970129     C                   Z-ADD     PI87          SPA87A
259300941012      * ---- PESI ----
259400090914     C                   Z-ADD     SKD(6)        SPAPFC
259500090914     C                   Z-ADD     SKD(7)        SPAPFE
259600090914     C                   Z-ADD     SKD(8)        SPAPFD
259700090914     C                   Z-ADD     SKD(9)        SPAPFh
259800090918     C                   Z-ADD     SKE(6)        SPAPOC
259900090918     C                   Z-ADD     SKE(7)        SPAPOE
260000090914     C                   Z-ADD     SKE(8)        SPAPOD
260100090914     C                   Z-ADD     SKE(9)        SPAPOh
260200941012     C                   Z-ADD     PETS          SPAPTA
260300941012      * ---- VOLUMI ----
260400090915     C                   Z-ADD     SKI(6)        SPAVPC
260500090915     C                   Z-ADD     SKI(7)        SPAVPE
260600090914     C                   Z-ADD     SKI(8)        SPAVPD
260700090914     C                   Z-ADD     SKI(9)        SPAVPh
260800941012      * ---- SPEDIZIONI --
260900090915     C                   Z-ADD     SKL(6)        SPASPC
261000090915     C                   Z-ADD     SKL(7)        SPASPE
261100090914     C                   Z-ADD     SKL(8)        SPASPD
261200090914     C                   Z-ADD     SKL(9)        SPASPh
261300090915     C                   Z-ADD     SKM(6)        SPASDC
261400090915     C                   Z-ADD     SKM(7)        SPASDE
261500090914     C                   Z-ADD     SKM(8)        SPASDD
261600090914     C                   Z-ADD     SKM(9)        SPASDh
261700090915     C                   Z-ADD     SKO(6)        SPASFC
261800090915     C                   Z-ADD     SKO(7)        SPASFE
261900090914     C                   Z-ADD     SKO(8)        SPASFD
262000090914     C                   Z-ADD     SKO(9)        SPASFh
262100090915     C                   Z-ADD     SKP(6)        SPATFC
262200090915     C                   Z-ADD     SKP(7)        SPATFE
262300090914     C                   Z-ADD     SKP(8)        SPATFD
262400090914     C                   Z-ADD     SKP(9)        SPATFh
262500941012      *
262600040921     c                   move      *blanks       spaftr
262700040921     c                   z-add     dateu         spadtr
262800970204     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI SCRIVO RECORD CON
262900970226     C* TIPO = 'F'
263000970226     C     WFES          IFEQ      'F'
263100970226     C     SPACPE        ANDEQ     *ZEROS
263200970204     C     SPACPC        ANDEQ     *ZEROS
263300970204     C     SPACPD        ANDEQ     *ZEROS
263400970204     C                   MOVE      'F'           SPATRE
263500970204     C                   END
263600090911     C                   WRITE     FISPA000
263700941012      *
263800941012      * --------------------- CONTO SERVIZI DI FILIALE -------------------------
263900941012      *
264000941012      * Finche sono qua (totali di filiale), scrivo anche i c/serv. di filiale
264100941012     C                   Z-ADD     SIMFEL        SPAFLE
264200941012     C                   Z-ADD     COMFIL        SPALNP
264300950210     C                   Z-ADD     DS1ADA        SPADRE
264400941012     C                   MOVE      'C'           SPATRE
264500941012      * ---- COLLI ----
264600090915     C                   Z-ADD     SKA(11)       SPACPC
264700090915     C                   Z-ADD     SKA(12)       SPACPE
264800090914     C                   Z-ADD     SKA(13)       SPACPD
264900090914     C                   Z-ADD     SKA(14)       SPACPh
265000970130     C                   Z-ADD     PB88          SPA88B
265100970130     C                   Z-ADD     PS88          SPA88A
265200970130     C                   Z-ADD     PB87          SPA87B
265300970130     C                   Z-ADD     PS87          SPA87A
265400941012      * ---- PESI ----
265500090915     C                   Z-ADD     SKD(11)       SPAPFC
265600090915     C                   Z-ADD     SKD(12)       SPAPFE
265700090914     C                   Z-ADD     SKD(13)       SPAPFD
265800090914     C                   Z-ADD     SKD(14)       SPAPFh
265900090921     C                   Z-ADD     SKE(11)       SPAPOC
266000090921     C                   Z-ADD     SKE(12)       SPAPOE
266100090914     C                   Z-ADD     SKE(13)       SPAPOD
266200090914     C                   Z-ADD     SKE(14)       SPAPOh
266300941012     C                   Z-ADD     *ZERO         SPAPTA
266400941012      * ---- VOLUMI ----
266500090915     C                   Z-ADD     SKI(11)       SPAVPC
266600090915     C                   Z-ADD     SKI(12)       SPAVPE
266700090914     C                   Z-ADD     SKI(13)       SPAVPD
266800090914     C                   Z-ADD     SKI(14)       SPAVPh
266900941012      * ---- SPEDIZIONI --
267000090915     C                   Z-ADD     SKL(11)       SPASPC
267100090915     C                   Z-ADD     SKL(12)       SPASPE
267200090914     C                   Z-ADD     SKL(13)       SPASPD
267300090914     C                   Z-ADD     SKL(14)       SPASPh
267400090915     C                   Z-ADD     SKM(11)       SPASDC
267500090915     C                   Z-ADD     SKM(12)       SPASDE
267600090914     C                   Z-ADD     SKM(13)       SPASDD
267700090914     C                   Z-ADD     SKM(14)       SPASDh
267800090915     C                   Z-ADD     SKO(11)       SPASFC
267900090915     C                   Z-ADD     SKO(12)       SPASFE
268000090914     C                   Z-ADD     SKO(13)       SPASFD
268100090914     C                   Z-ADD     SKO(14)       SPASFh
268200090915     C                   Z-ADD     SKP(11)       SPATFC
268300090915     C                   Z-ADD     SKP(12)       SPATFE
268400090914     C                   Z-ADD     SKP(13)       SPATFD
268500090914     C                   Z-ADD     SKP(14)       SPATFh
268600040921     c                   clear                   spaftr
268700040921     c                   move      dateu         spadtr
268800941012      *
268900970226     C* SE GIORNO FESTIVO E NON SONO PARTITI COLLI NON SCRIVO NIENTE
269000970226     C     WFES          IFEQ      'F'
269100970226     C     SPACPE        ANDEQ     *ZEROS
269200970226     C     SPACPC        ANDEQ     *ZEROS
269300970226     C     SPACPD        ANDEQ     *ZEROS
269400970226     C                   ELSE
269500090911     C                   WRITE     FISPA000
269600970226     C                   END
269700941012      *
269800941012     C                   ENDSR
269900941012      ****************************************************************
270000941012      * Scrivo in archivio il riepilogo per transiti
270100941012      ****************************************************************
270200941012     C     WRITRA        BEGSR
270300941012      *
270400941012     C                   Z-ADD     SIMFEL        SPAFLE
270500941012     C                   Z-ADD     SIMFEL        SPALNP
270600950210     C                   Z-ADD     DS1ADA        SPADRE
270700941012     C                   MOVE      'T'           SPATRE
270800941012      * ---- COLLI ----
270900970130     C                   Z-ADD     XKA(2)        SPACPE
271000970130     C                   Z-ADD     XKA(1)        SPACPC
271100970130     C                   Z-ADD     XKA(3)        SPACPD
271200090914     C                   Z-ADD     XKA(4)        SPACPh
271300970130     C* N.B.:Nei transiti non si distinguono i colli disk B dagli altri
271400970130     C                   MOVE      *ZEROS        SPA88B
271500970130     C                   Z-ADD     TRPI88        SPA88A
271600970130     C                   MOVE      *ZEROS        SPA87B
271700970130     C                   Z-ADD     TRPI87        SPA87A
271800941012      * ---- PESI ----
271900941012     C                   Z-ADD     XKD(2)        SPAPFE
272000970203     C                   Z-ADD     XKD(1)        SPAPFC
272100970203     C                   Z-ADD     XKD(3)        SPAPFD
272200090914     C                   Z-ADD     XKD(4)        SPAPFh
272300970130     C                   Z-ADD     XKE(2)        SPAPOE
272400970130     C                   Z-ADD     XKE(1)        SPAPOC
272500970130     C                   Z-ADD     XKE(3)        SPAPOD
272600090914     C                   Z-ADD     XKE(4)        SPAPOh
272700941012     C                   Z-ADD     TRPETS        SPAPTA
272800941012      * ---- VOLUMI ----
272900970130     C                   Z-ADD     XKI(2)        SPAVPE
273000970130     C                   Z-ADD     XKI(1)        SPAVPC
273100970130     C                   Z-ADD     XKI(3)        SPAVPD
273200090914     C                   Z-ADD     XKI(4)        SPAVPh
273300941012      * ---- SPEDIZIONI --
273400970130     C                   Z-ADD     XKL(2)        SPASPE
273500970130     C                   Z-ADD     XKL(1)        SPASPC
273600970130     C                   Z-ADD     XKL(3)        SPASPD
273700090914     C                   Z-ADD     XKL(4)        SPASPh
273800970130     C                   Z-ADD     *ZERO         SPASDE
273900970130     C                   Z-ADD     *ZERO         SPASDC
274000970130     C                   Z-ADD     *ZERO         SPASDD
274100970130     C                   Z-ADD     *ZERO         SPASFE
274200970130     C                   Z-ADD     *ZERO         SPASFC
274300970130     C                   Z-ADD     *ZERO         SPASFD
274400941012      *
274500040921     c                   clear                   spaftr
274600040921     c                   z-add     dateu         spadtr
274700941012      *
274800970130     C     SPACPE        IFGT      *ZERO
274900970130     C     SPACPC        ORGT      *ZERO
275000970130     C     SPACPD        ORGT      *ZERO
275100090911     C                   WRITE     FISPA000
275200941012     C                   ENDIF
275300941012      *
275400941012     C                   ENDSR
275500941012      ****************************************************************
275600090911      * Routine di RISTAMPA: viene semplicemente letto il file FISPA00F
275700941012      * e stampato, senza alcuna elaborazione di dati.
275800941012      * E' quindi possibile ristampare piu' giorni accorpandoli insieme.
275900941012      * Le routine di RISTAMPA sono simili a quelle del pgm FNLSC2R.
276000941012      ****************************************************************
276100941012     C     RISTAM        BEGSR
276200050829     c                   setoff                                       3534
276300941012      *
276400941012      * Verifico se richiesta una specifica area
276500950210     C     DS1FEL        COMP      *ZERO                                  34
276600050829     c
276700050829     c* Se sono in filiale imposto = simfel
276800050829     c                   if        ds1fel=0 and simfel>0
276900050829     c                   eval      ds1fel=simfel
277000050829     c                   setoff                                       34
277100050829     c                   endif
277200050829     c
277300050829     c* Se devo elaborare una sola linea di partenza, leggo con
277400050829     c*  la linea
277500050829     c                   if        lin(2)=0 and lin(1)>0
277600050829     c                   eval      klin=lin(1)
277700050829     c                   eval      kdre=ds1ada
277800050829     c                   seton                                        35
277900050829     c                   endif
278000050829     c
278100020813     C                   MOVE      *ZEROS        XD
278200941012      *
278300941012     C                   Z-ADD     *ZERO         $PRIM1            1 0
278400941012     C                   Z-ADD     *ZERO         $FILPR            3 0
278500941012     C                   Z-ADD     *ZERO         $FI2PR            3 0
278600941012      *
278700090911     C  n35DS1FEL        SETLL     FISPA01L
278800090911     C   35kspa          SETLL     FISPA01L
278900941012     C                   DO        *HIVAL
279000050829     c* 35 on - lettura con lnp
279100050829     c                   if        *in35
279200090911     C     kspa2         READE(N)  FISPA01L                               33
279300050829     c                   else
279400090911     C  N34DS1FEL        READE(N)  FISPA01L                               33
279500090911     C   34              READ(N)   FISPA01L                               33
279600050829     c                   endif
279700941012      *
279800970130     C  N33SPADRE        IFGE      DS1ADA
279900970130     C     SPADRE        ANDLE     DS1AAL
280000941012      *
280100941012      * La linea deve essere presente in LIN
280200050829     c                   if        not *in35 and *in34
280300941012     C     SPALNP        LOOKUP    LIN                                    32
280400050829     c                   else
280500050829     c                   seton                                        32
280600050829     c                   endif
280700941012      *
280800941012     C     *IN32         IFEQ      *ON
280900941012      *
281000941012     C     SPATRE        IFEQ      *BLANK
281100941012      *
281200941012      * Al primo giro memorizzo il "precedente"
281300941012     C     $PRIM1        IFEQ      *ZERO
281400941012     C                   Z-ADD     SPAFLE        $FI2PR
281500941012     C                   Z-ADD     SPALNP        $FILPR
281600941012     C                   Z-ADD     1             $PRIM1
281700941012     C                   ENDIF
281800941012      *
281900941012      * Se cambia filiale stampo i totali per filiale
282000941012     C     SPALNP        IFNE      $FILPR
282100020813     C                   ADD       1             XD
282200941012     C                   Z-ADD     $FILPR        COMFIL
282300941012     C                   MOVE      *BLANKS       DESLNP
282400941012     C     $FILPR        CHAIN     AZORG01L                           99
282500970203     C     *IN99         IFEQ      *OFF
282600970203     C     ORGDE5        IFNE      *BLANKS
282700970203     C                   MOVEL     ORGDE5        DESLNP
282800970203     C                   ELSE
282900970203     C                   MOVEL     ORGDES        DESLNP
283000970203     C                   END
283100970203     C                   END
283200941012     C                   EXSR      TOTLNP
283300941012     C                   Z-ADD     SPALNP        $FILPR
283400941012     C                   ENDIF
283500941012      *
283600941012      * Se cambia area stampo i totali per c/servizio
283700941012     C     SPAFLE        IFNE      $FI2PR
283800941012     C                   EXSR      TOTCS
283900941012     C                   EXSR      TOTARE
284000000308     C  N08              XFOOT     XKA           TOTXKA            7 0
284100000308     C     *IN08         IFEQ      *ON
284200000308     C     TOTXKA        ORGT      0
284300000308     C                   EXSR      TOTTRA
284400000308     C                   ENDIF
284500941012     C                   Z-ADD     SPAFLE        $FI2PR
284600941012     C                   ENDIF
284700000308      *
284800941012     C                   ENDIF
284900941012      *
285000941012      * Aggiorno i totali
285100941012     C                   EXSR      SERTRA
285200941012      *
285300941012     C                   ENDIF
285400941012     C                   ENDIF
285500941012      *
285600941012     C  N33              ENDDO
285700941012      *
285800941012      * Stampo l'ultimo totale filiale ed il c/servizio
285900941012     C                   Z-ADD     $FILPR        COMFIL
286000941012     C                   MOVE      *BLANKS       DESLNP
286100941012     C     $FILPR        CHAIN     AZORG01L                           99
286200970203     C     *IN99         IFEQ      *OFF
286300970203     C     ORGDE5        IFNE      *BLANKS
286400970203     C                   MOVEL     ORGDE5        DESLNP
286500970203     C                   ELSE
286600970203     C                   MOVEL     ORGDES        DESLNP
286700970203     C                   END
286800970203     C                   END
286900941012     C                   EXSR      TOTLNP
287000941012     C                   EXSR      TOTCS
287100941012     C                   EXSR      TOTARE
287200941012      *
287300941012      * Stampo il transito
287400000308     C  N08              XFOOT     XKA           TOTXKA            7 0
287500000308     C     *IN08         IFEQ      *ON
287600000308     C     TOTXKA        ORGT      0
287700000308     C                   EXSR      TOTTRA
287800000308     C                   ENDIF
287900941012      *
288000941012     C     ENDRIS        ENDSR
288100941012      *****************************************************************
288200941012      * RISTAMPA: mantengo aggiornati i totali
288300941012      *****************************************************************
288400941012     C     SERTRA        BEGSR
288500941012      *
288600941012     C     SPATRE        IFEQ      *BLANK
288700941012      * Colli
288800090914     C                   ADD       SPACPC        SKA(6)
288900090914     C                   ADD       SPACPE        SKA(7)
289000090914     C                   ADD       SPACPD        SKA(8)
289100090914     C                   ADD       SPACPh        SKA(9)
289200970130     C                   ADD       SPA88B        PB88F
289300970130     C                   ADD       SPA88A        PI88
289400970130     C                   ADD       SPA87B        PB87F
289500970130     C                   ADD       SPA87A        PI87
289600941012      * Pesi
289700090914     C                   ADD       SPAPFC        SKD(6)
289800090914     C                   ADD       SPAPFE        SKD(7)
289900090914     C                   ADD       SPAPFD        SKD(8)
290000090914     C                   ADD       SPAPFh        SKD(9)
290100090914
290200090914     C                   ADD       SPAPOC        SKE(6)
290300090914     C                   ADD       SPAPOE        SKE(7)
290400090914     C                   ADD       SPAPOD        SKE(8)
290500090914     C                   ADD       SPAPOh        SKE(9)
290600090914     c
290700090914     C     SKD(6)        ADD       SKE(6)        SKF(6)
290800090914     C     SKD(7)        ADD       SKE(7)        SKF(7)
290900090914     C     SKD(8)        ADD       SKE(8)        SKF(8)
291000090914     C     SKD(9)        ADD       SKE(9)        SKF(9)
291100941012     C                   ADD       SPAPTA        PETS
291200941012      * Volumi
291300090914     C                   ADD       SPAVPC        SKI(6)
291400090914     C                   ADD       SPAVPE        SKI(7)
291500090914     C                   ADD       SPAVPD        SKI(8)
291600090914     C                   ADD       SPAVPh        SKI(9)
291700941012      * Spedizioni
291800090914     C                   ADD       SPASPC        SKL(6)
291900090914     C                   ADD       SPASPE        SKL(7)
292000090914     C                   ADD       SPASPD        SKL(8)
292100090914     C                   ADD       SPASPh        SKL(9)
292200090914
292300090914     C                   ADD       SPASDC        SKM(6)
292400090914     C                   ADD       SPASDE        SKM(7)
292500090914     C                   ADD       SPASDD        SKM(8)
292600090914     C                   ADD       SPASDh        SKM(9)
292700090914
292800090914     C     SPASFC        ADD       SPATFC        SKN(6)
292900090914     C     SPASFE        ADD       SPATFE        SKN(7)
293000090914     C     SPASFD        ADD       SPATFD        SKN(8)
293100090914     C     SPASFh        ADD       SPATFh        SKN(9)
293200941012     C                   ENDIF
293300941012      *
293400941012     C     SPATRE        IFEQ      'C'
293500941012      * Colli
293600970130     C                   ADD       SPACPC        SKA(1)
293700970130     C                   ADD       SPACPE        SKA(2)
293800970130     C                   ADD       SPACPD        SKA(3)
293900090914     C                   ADD       SPACPh        SKA(4)
294000970130     C                   ADD       SPA88B        PB88CS
294100970130     C                   ADD       SPA88B        PB88
294200970130     C                   ADD       SPA88A        PI88CS
294300970130     C                   ADD       SPA88A        PS88
294400970130     C                   ADD       SPA87B        PB87CS
294500970130     C                   ADD       SPA87B        PB87
294600970130     C                   ADD       SPA87A        PI87CS
294700970130     C                   ADD       SPA87A        PS87
294800941012      * Pesi
294900970130     C                   ADD       SPAPFC        SKD(1)
295000970130     C                   ADD       SPAPFE        SKD(2)
295100970130     C                   ADD       SPAPFD        SKD(3)
295200090914     C                   ADD       SPAPFh        SKD(4)
295300970130     C                   ADD       SPAPOC        SKE(1)
295400970130     C                   ADD       SPAPOE        SKE(2)
295500970130     C                   ADD       SPAPOD        SKE(3)
295600090914     C                   ADD       SPAPOh        SKE(4)
295700970130     C     SKD(1)        ADD       SKE(1)        SKF(1)
295800970130     C     SKD(2)        ADD       SKE(2)        SKF(2)
295900970130     C     SKD(3)        ADD       SKE(3)        SKF(3)
296000090914     C     SKD(4)        ADD       SKE(4)        SKF(4)
296100941012      * Volumi
296200970130     C                   ADD       SPAVPC        SKI(1)
296300970130     C                   ADD       SPAVPE        SKI(2)
296400970130     C                   ADD       SPAVPD        SKI(3)
296500090914     C                   ADD       SPAVPh        SKI(4)
296600941012      * Spedizioni
296700970130     C                   ADD       SPASPC        SKL(1)
296800970130     C                   ADD       SPASPE        SKL(2)
296900970130     C                   ADD       SPASPD        SKL(3)
297000090914     C                   ADD       SPASPh        SKL(4)
297100970130     C                   ADD       SPASDC        SKM(1)
297200970130     C                   ADD       SPASDE        SKM(2)
297300970130     C                   ADD       SPASDD        SKM(3)
297400090914     C                   ADD       SPASDh        SKM(4)
297500970228     C     SPATFC        ADD       SPASFC        W0010
297600970228     C     SPATFE        ADD       SPASFE        W0020
297700970228     C     SPATFD        ADD       SPASFD        W0030
297800090914     C     SPATFh        ADD       SPASFh        W0040
297900970228     C                   ADD       W0010         SKN(1)
298000970228     C                   ADD       W0020         SKN(2)
298100970228     C                   ADD       W0030         SKN(3)
298200090914     C                   ADD       W0040         SKN(4)
298300941012     C                   ENDIF
298400941012      *
298500941012     C     SPATRE        IFEQ      'T'
298600970130     C                   ADD       SPACPC        XKA(1)
298700970130     C                   ADD       SPACPE        XKA(2)
298800970130     C                   ADD       SPACPD        XKA(3)
298900090914     C                   ADD       SPACPh        XKA(4)
299000970130     C                   ADD       SPA88A        TRPI88
299100970130     C                   ADD       SPA87A        TRPI87
299200941012      * Pesi
299300970130     C                   ADD       SPAPFC        XKD(1)
299400970130     C                   ADD       SPAPFE        XKD(2)
299500970130     C                   ADD       SPAPFD        XKD(3)
299600090914     C                   ADD       SPAPFh        XKD(4)
299700970130     C                   ADD       SPAPOC        XKE(1)
299800970130     C                   ADD       SPAPOE        XKE(2)
299900970130     C                   ADD       SPAPOD        XKE(3)
300000090914     C                   ADD       SPAPOh        XKE(4)
300100970228     C     XKD(1)        ADD       XKE(1)        XKF(1)
300200970228     C     XKD(2)        ADD       XKE(2)        XKF(2)
300300970228     C     XKD(3)        ADD       XKE(3)        XKF(3)
300400090914     C     XKD(4)        ADD       XKE(4)        XKF(4)
300500941012     C                   ADD       SPAPTA        TRPETS
300600941012      * Volumi
300700970130     C                   ADD       SPAVPC        XKI(1)
300800970130     C                   ADD       SPAVPE        XKI(2)
300900970130     C                   ADD       SPAVPD        XKI(3)
301000090914     C                   ADD       SPAVPh        XKI(4)
301100941012      * Spedizioni
301200970130     C                   ADD       SPASPC        XKL(1)
301300970130     C                   ADD       SPASPE        XKL(2)
301400970130     C                   ADD       SPASPD        XKL(3)
301500090914     C                   ADD       SPASPh        XKL(4)
301600941012     C                   ENDIF
301700941012      *
301800941012     C                   ENDSR
301900941012      ****************************************************************
302000941012      * Verifica se vi sono dei fogli viaggio con data partenza compresa
302100941012      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
302200941012      ****************************************************************
302300941012     C     FOGVIA        BEGSR
302400941012      *
302500950404     C  N36DS1RIS        IFEQ      'S'
302600941012      *
302700020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
302800020813     ***C     *LOVAL        SETLL     FNFGV04L
302900020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
303000020813     C     SIMfel        setll     FNFGV06L
303100941012     C                   DO        *HIVAL
303200020813     ***C                   READ      FNFGV04L                               31
303300020813     C     SIMfel        reade     FNFGV06L                               31
303400950210     C  N31FGVDTP        IFGE      DS1ADA
303500950210     C     FGVDTP        ANDLE     DS1AAL
303600941012     C                   SETON                                        31
303700941012     C                   EXCEPT    FVAPER
303800941012     C                   ENDIF
303900941012     C  N31              ENDDO
304000941012      *
304100941012     C                   ENDIF
304200941012      *
304300941012     C                   ENDSR
304400940103      ****************************************************************
304500941012      * ----------------?    TESTATA  DI  STAMPA    ?-----------------
304600930928     O*PRIMA RIGA SE C'E' SOLO 'DATA DA'
304700930928     OPRTF198   E   N03      TESTA            02
304800930928     O                       SK(1)               66
304900930928     O                       SK(2)              132
305000930928     O                       SK(4)              198
305100930928     O                       RSUT                20
305200941012     O                       STRIP               48
305300950210     O                       DS1GDA             113 '  /  /    '
305400950210     O                       WDATE              181 '  /  /    '
305500930928     O                       PAGE          Z    198
305600930928     O*PRIMA RIGA SE C'E' 'DATA DA' E 'DATA A'
305700930928     O          E    03      TESTA            02
305800930928     O                       SK(1)               66
305900930928     O                       SK(3)              132
306000930928     O                       SK(4)              198
306100930928     O                       RSUT                20
306200941012     O                       STRIP               48
306300950210     O                       DS1GDA             113 '  /  /    '
306400950210     O                       DS1GAL             128 '  /  /    '
306500950210     O                       WDATE              181 '  /  /    '
306600930928     O                       PAGE          Z    198
306700930928     O*RIGA IN CUI C'E' SOLO UTIME
306800930928     O          E            TESTA            03
306900950111     O                       KNSIF               10
307000950111     O                       KNMUS               21
307100941012     O                       STRIP1              48
307200950210     O                       UTIME              180 '  :  :  '
307300930928     O*TRATTINI
307400930928     O          E            TESTA            04
307500930928     O                       SK(5)               66
307600930928     O                       SK(5)              132
307700970306     O                       SK(18)             198
307800930928     O*PRIMA RIGA DI COSTANTI
307900930928     O          E            TESTA            05
308000930928     O                       SK(6)               66
308100970131     O                       SK(9)              132
308200970131     O                       SK(12)             198
308300930928     O*SECONDA RIGA DI COSTANTI
308400930928     O          E            TESTA            06
308500970131     O                       SK(7)               66
308600930928     O                       SK(10)             132
308700970131     O                       SK(13)             198
308800930928     O*TERZA RIGA DI COSTANTI
308900930928     O          E            TESTA            07
309000970131     O                       SK(8)               66
309100970131     O                       SK(11)             132
309200930928     O                       SK(14)             198
309300930928     O*TRATTINI
309400970131     O          E            TESTA            08
309500930928     O                       SK(5)               66
309600930928     O                       SK(5)              132
309700970306     O                       SK(18)             198
309800941012      * ----------------?    DETTAGLIO DI FILIALE    ?----------------
309900930928     O****1°RIGA DEL DETTAGLIO FILIALE = TOTALI
310000930928     O**COLLI
310100930929     O          E            DETFIL      2
310200970131     O                       DESLNP              10
310300970131     O                       SKA456        Z     18
310400970131     O*COLLI NO SPUNT. IN PART. DISK B
310500970131     O                       PB88F         Z     26
310600970131     O                       PC88B         4     33
310700970131     O                       PER3B               34
310800970131     O*COLLI NO SPUNT. IN PART. ALTRI
310900970131     O                       PI88          Z     42
311000970131     O                       PC88          4     49
311100970131     O                       PER3                50
311200970131     O*COLLI NO SPUNT. IN PART. TOTALE
311300970131     O                       PI88T         Z     58
311400970131     O                       PC88T         4     65
311500970131     O                       PER3T               66
311600970131     O*COLLI PISTOLA 87 (EX 88) DISK B
311700970131     O                       PB87F         Z     74
311800970131     O                       PC87B         4     81
311900970131     O                       PER34B              82
312000970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
312100970131     O                       PI87          Z     90
312200970131     O                       PC87          4     97
312300970131     O                       PER34               98
312400970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
312500970131     O                       PI87T         Z    106
312600970131     O                       PC87T         4    113
312700970131     O                       PER34T             114
312800930928     O**PESO
312900970131     O                       SKD456        4    124
313000970131     O                       SKE456        4    134
313100970131     O                       SKF456        4    144
313200930928     O**PESO TASSABILE
313300970131     O                       PETS          4    155
313400930928     O**VOLUME
313500970131     O                       SKI456        4    165
313600930928     O**SPEDIZIONI
313700970131     O                       SKL456        Z    174
313800970131     O                       SKM456        Z    182
313900090914     O*
314000090914     O****  RIGA DEL DETTAGLIO FILIALE = "H" - h1030
314100090914     O**COLLI
314200090914     O          E    10      DETFIL      1
314300090914     o                       wdtsp(04)           11
314400090914     O                       SKA(9)        Z     18
314500090914     O**PESO
314600090914     O                       SKD(9)        4    124
314700090914     O                       SKE(9)        4    134
314800090914     O                       SKF(9)        4    144
314900090914     O**VOLUME
315000090914     O                       SKI(9)        4    165
315100090914     O**SPEDIZIONI
315200090914     O                       SKL(9)        Z    174
315300090914     O                       SKM(9)        Z    182
315400090914     O*
315500930928     O*
315600090914     O****2°RIGA DEL DETTAGLIO FILIALE = "E" - priority
315700930928     O**COLLI
315800930929     O          E            DETFIL      1
315900090605     O**!!!                                      11 'ESPR'
316000090605     o                       wdtsp(02)           11
316100090914     O                       SKA(7)        Z     18
316200930928     O**PESO
316300090914     O                       SKD(7)        4    124
316400090914     O                       SKE(7)        4    134
316500090914     O                       SKF(7)        4    144
316600930928     O**VOLUME
316700090914     O                       SKI(7)        4    165
316800930928     O**SPEDIZIONI
316900090914     O                       SKL(7)        Z    174
317000090914     O                       SKM(7)        Z    182
317100931011     O*
317200090914     O****3°RIGA DEL DETTAGLIO FILIALE = "D" - DISTRIBUZIONE
317300930928     O**COLLI
317400930929     O          E            DETFIL      1
317500090605     O**!!!                                      11 'DIST'
317600090605     o                       wdtsp(03)           11
317700090914     O                       SKA(8)        Z     18
317800930928     O**PESO
317900090914     O                       SKD(8)        4    124
318000090914     O                       SKE(8)        4    134
318100090914     O                       SKF(8)        4    144
318200930928     O**VOLUME
318300090914     O                       SKI(8)        4    165
318400930928     O**SPEDIZIONI
318500090914     O                       SKL(8)        Z    174
318600090914     O                       SKM(8)        Z    182
318700940323     O*
318800090914     O****4°RIGA DEL DETTAGLIO FILIALE = "C" - express
318900940323     O**COLLI
319000940323     O          E            DETFIL      1
319100090605     O**!!!                                      11 'CORR'
319200090605     o                       wdtsp(01)           11
319300090914     O                       SKA(6)        Z     18
319400940323     O**PESO
319500090914     O                       SKD(6)        4    124
319600090914     O                       SKE(6)        4    134
319700090914     O                       SKF(6)        4    144
319800940323     O**VOLUME
319900090914     O                       SKI(6)        4    165
320000940323     O**SPEDIZIONI
320100090914     O                       SKL(6)        Z    174
320200090914     O                       SKM(6)        Z    182
320300941012      * ----------?    RIEPILOGO C/SERVIZI PER FILIALE    ?-----------
320400930929     O****1°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = TOTALI
320500930929     O**COLLI
320600970131     O          E            DETCS       2
320700970131     O                                           10 'C/SERVIZI '
320800970131     O                       SKA123        Z     18
320900970131     O*COLLI NO SPUNT. IN PART. DISK B
321000970131     O                       PB88CS        Z     26
321100970131     O                       PC88CB        4     33
321200970131     O                       PER3B               34
321300970131     O*COLLI NO SPUNT. IN PART. ALTRI
321400970131     O                       PI88CS        Z     42
321500970131     O                       PC88CS        4     49
321600970131     O                       PER3                50
321700970131     O*COLLI NO SPUNT. IN PART. TOTALE
321800970131     O                       P88CST        Z     58
321900970131     O                       PC88CT        4     65
322000970131     O                       PER3T               66
322100970131     O*COLLI PISTOLA 87 (EX 88) DISKB
322200970131     O                       PB87CS        Z     74
322300970131     O                       PC87CB        4     81
322400970131     O                       PER34B              82
322500970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
322600970131     O                       PI87CS        Z     90
322700970131     O                       PC87CS        4     97
322800970131     O                       PER34               98
322900970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
323000970131     O                       P87CST        Z    106
323100970131     O                       PC87CT        4    113
323200970131     O                       PER34T             114
323300930929     O**PESO
323400970131     O                       SKD123        4    124
323500970131     O                       SKE123        4    134
323600970131     O                       SKF123        4    144
323700930929     O**PESO TASSABILE (PER C/SERVIZI NON C'E').
323800930929     O**VOLUME
323900970131     O                       SKI123        4    165
324000930929     O**SPEDIZIONI
324100970131     O                       SKL123        Z    174
324200970131     O                       SKM123        Z    182
324300930929     O*
324400090914     O****  RIGA DEL DETTAGLIO FILIALE C/SERVIZI = "H" - h 1030
324500090914     O**COLLI
324600090914     O          E    10      DETCS       1
324700090914     o                       wdtsp(04)           11
324800090914     O                       SKA(4)        Z     18
324900090914     O**PESO
325000090914     O                       SKD(4)        4    124
325100090914     O                       SKE(4)        4    134
325200090914     O                       SKF(4)        4    144
325300090914     O**VOLUME
325400090914     O                       SKI(4)        4    165
325500090914     O**SPEDIZIONI
325600090914     O                       SKL(4)        Z    174
325700090914     O                       SKM(4)        Z    182
325800090914     O*
325900090914     O****2°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = "E" - priority
326000930929     O**COLLI
326100930929     O          E            DETCS       1
326200090605     O**!!!                                      11 'ESPR'
326300090605     o                       wdtsp(02)           11
326400970131     O                       SKA(2)        Z     18
326500930929     O**PESO
326600970131     O                       SKD(2)        4    124
326700970131     O                       SKE(2)        4    134
326800970131     O                       SKF(2)        4    144
326900930929     O**VOLUME
327000970131     O                       SKI(2)        4    165
327100930929     O**SPEDIZIONI
327200970131     O                       SKL(2)        Z    174
327300970131     O                       SKM(2)        Z    182
327400931011     O*
327500090914     O****3°RIGA DEL DETTAGLIO FILIALE C/SERVIZI = "D" - DISTRIBUZIONE
327600930929     O**COLLI
327700930929     O          E            DETCS       1
327800090605     O**!!!                                      11 'DIST'
327900090605     o                       wdtsp(03)           11
328000970131     O                       SKA(3)        Z     18
328100930929     O**PESO
328200970131     O                       SKD(3)        4    124
328300970131     O                       SKE(3)        4    134
328400970131     O                       SKF(3)        4    144
328500930929     O**VOLUME
328600970131     O                       SKI(3)        4    165
328700930929     O**SPEDIZIONI
328800970131     O                       SKL(3)        Z    174
328900970131     O                       SKM(3)        Z    182
329000940323     O*
329100090914     O****4°RIGA DEL DETTAGLIO FILIALE C/SERVIZI= "C"- express
329200940323     O**COLLI
329300940323     O          E            DETCS       1
329400090605     O**!!!                                      11 'CORR'
329500090605     o                       wdtsp(01)           11
329600970131     O                       SKA(1)        Z     18
329700940323     O**PESO
329800970131     O                       SKD(1)        4    124
329900970131     O                       SKE(1)        4    134
330000970131     O                       SKF(1)        4    144
330100940323     O**VOLUME
330200970131     O                       SKI(1)        4    165
330300940323     O**SPEDIZIONI
330400970131     O                       SKL(1)        Z    174
330500970131     O                       SKM(1)        Z    182
330600941012      * ----------------?    RIEPILOGO PER AREA    ?------------------
330700941012     O*TRATTINI DI SEPARAZIONE TRA I  C/SERVIZI E L'AREA
330800941012     O          E            DETARE      1
330900941012     O                       SK(5)               66
331000941012     O                       SK(5)              132
331100970306     O                       SK(18)             198
331200090914     o
331300930930     O****1°RIGA DEL DETTAGLIO AREA = TOTALI
331400930930     O**COLLI
331500931011     O          E            DETARE      1
331600970131     O                                           10 'TOTALE    '
331700970131     O                       AKA123        Z     18
331800970131     O*COLLI NO SPUNT. IN PART. DISK B
331900970131     O                       ARPB88        Z     26
332000970131     O                       APC88B        4     33
332100970131     O                       PER3B               34
332200970131     O*COLLI NO SPUNT. IN PART. ALTRI
332300970131     O                       ARPI88        Z     42
332400970131     O                       ARPC88        4     49
332500970131     O                       PER3                50
332600970131     O*COLLI NO SPUNT. IN PART. TOTALE
332700970131     O                       ARP88T        Z     58
332800970131     O                       APC88T        4     65
332900970131     O                       PER3T               66
333000970131     O*COLLI PISTOLA 87 (EX 88) DISK B
333100970131     O                       ARPB87        Z     74
333200970131     O                       APC87B        4     81
333300970131     O                       PER34B              82
333400970131     O*COLLI PISTOLA 87 (EX 88) ALTRI
333500970131     O                       ARPI87        Z     90
333600970131     O                       ARPC87        4     97
333700970131     O                       PER34               98
333800970131     O*COLLI PISTOLA 87 (EX 88) TOTALE
333900970131     O                       ARP87T        Z    106
334000970131     O                       APC87T        4    113
334100970131     O                       PER34T             114
334200930930     O**PESO
334300970131     O                       AKD123        4    124
334400970131     O                       AKE123        4    134
334500970131     O                       AKF123        4    144
334600930930     O**PESO TASSABILE
334700970131     O                       ARPETS        4    155
334800930930     O**VOLUME
334900970131     O                       AKI123        4    165
335000930930     O**SPEDIZIONI
335100970131     O                       AKL123        Z    174
335200970131     O                       AKM123        Z    182
335300930930     O*
335400090914     O****  RIGA DEL DETTAGLIO AREA = "H"-h10 30
335500930930     O**COLLI
335600090914     O          E    10      DETARE      1
335700090914     o                       wdtsp(04)           11
335800090914     O                       AKA(4)        Z     18
335900930930     O**PESO
336000090914     O                       AKD(4)        4    124
336100090914     O                       AKE(4)        4    134
336200090914     O                       AKF(4)        4    144
336300930930     O**VOLUME
336400090914     O                       AKI(4)        4    165
336500930930     O**SPEDIZIONI
336600090914     O                       AKL(4)        Z    174
336700090914     O                       AKM(4)        Z    182
336800090914     O*
336900090914     O****2°RIGA DEL DETTAGLIO AREA = "E"-priority
337000090914     O**COLLI
337100090914     O          E            DETARE      1
337200090914     O**!!!                                      11 'ESPR'
337300090914     o                       wdtsp(02)           11
337400090914     O                       AKA(2)        Z     18
337500090914     O**PESO
337600090914     O                       AKD(2)        4    124
337700090914     O                       AKE(2)        4    134
337800090914     O                       AKF(2)        4    144
337900090914     O**VOLUME
338000090914     O                       AKI(2)        4    165
338100090914     O**SPEDIZIONI
338200090914     O                       AKL(2)        Z    174
338300090914     O                       AKM(2)        Z    182
338400931011     O*
338500090914     O****3°RIGA DEL DETTAGLIO AREA = "D" -DISTRIBUZIONE
338600930930     O**COLLI
338700940324     O          E            DETARE      1
338800090605     O**!!!                                      11 'DIST'
338900090605     o                       wdtsp(03)           11
339000970131     O                       AKA(3)        Z     18
339100930930     O**PESO
339200970131     O                       AKD(3)        4    124
339300970131     O                       AKE(3)        4    134
339400970131     O                       AKF(3)        4    144
339500930930     O**VOLUME
339600970131     O                       AKI(3)        4    165
339700930930     O**SPEDIZIONI
339800970131     O                       AKL(3)        Z    174
339900970131     O                       AKM(3)        Z    182
340000940323     O*
340100090914     O****4°RIGA DEL DETTAGLIO AREA = "C"- express
340200940323     O**COLLI
340300940324     O          E            DETARE      1  1
340400090605     O**!!!                                      11 'CORR'
340500090605     o                       wdtsp(01)           11
340600970203     O                       AKA(1)        Z     18
340700940323     O**PESO
340800970203     O                       AKD(1)        4    124
340900970203     O                       AKE(1)        4    134
341000970203     O                       AKF(1)        4    144
341100940323     O**VOLUME
341200970203     O                       AKI(1)        4    165
341300940323     O**SPEDIZIONI
341400970203     O                       AKL(1)        Z    174
341500970203     O                       AKM(1)        Z    182
341600941012      * ----------------?    RIEPILOGO TRANSITI    ?------------------
341700931008     O****1°RIGA DEL DETTAGLIO TRANSITI = TOTALI
341800931008     O**COLLI
341900970203     O          E            DETTRA      1
342000970203     O                                           10 'TRANSITI  '
342100970203     O                       XKA123        Z     18
342200931008     O*COLLI NO SPUNT. IN PART.
342300970203     O                       TRPI88        Z     58
342400970203     O                       TRPC88        4     65
342500970203     O                       PER3                66
342600950622     O*COLLI PISTOLA 87 (EX 88 E 89)
342700970203     O                       TRPI87        Z    106
342800970203     O                       TRPC87        4    113
342900970203     O                       PER34              114
343000931008     O**PESO
343100970203     O                       XKD123        4    124
343200970203     O                       XKE123        4    134
343300970203     O                       XKF123        4    144
343400931008     O**PESO TASSABILE
343500970203     O                       TRPETS        4    155
343600931008     O**VOLUME
343700970203     O                       XKI123        4    165
343800931008     O**SPEDIZIONI
343900970203     O                       XKL123        Z    174
344000931008     O*
344100090914     O****  RIGA DEL DETTAGLIO TRANSITI = "H" - h10 30
344200090914     O**COLLI
344300090914     O          E    10      DETTRA      1
344400090914     o                       wdtsp(04)           11
344500090914     O                       XKA(4)        Z     18
344600090914     O**PESO
344700090914     O                       XKD(4)        4    124
344800090914     O                       XKE(4)        4    134
344900090914     O                       XKF(4)        4    144
345000090914     O**VOLUME
345100090914     O                       XKI(4)        4    165
345200090914     O**SPEDIZIONI
345300090914     O                       XKL(4)        Z    174
345400090914     O*
345500090914     O****2°RIGA DEL DETTAGLIO TRANSITI = "E" - priority
345600931008     O**COLLI
345700931008     O          E            DETTRA      1
345800090605     O**!!!                                      11 'ESPR'
345900090605     o                       wdtsp(02)           11
346000970203     O                       XKA(2)        Z     18
346100931008     O**PESO
346200970203     O                       XKD(2)        4    124
346300970203     O                       XKE(2)        4    134
346400970203     O                       XKF(2)        4    144
346500931008     O**VOLUME
346600970203     O                       XKI(2)        4    165
346700931008     O**SPEDIZIONI
346800970203     O                       XKL(2)        Z    174
346900931011     O*
347000090914     O****3°RIGA DEL DETTAGLIO TRANSITI = "D" -DISTRIBUZIONE
347100931008     O**COLLI
347200931008     O          E            DETTRA      1
347300090605     O**!!!                                      11 'DIST'
347400090605     o                       wdtsp(03)           11
347500970203     O                       XKA(3)        Z     18
347600931008     O**PESO
347700970203     O                       XKD(3)        4    124
347800970203     O                       XKE(3)        4    134
347900970203     O                       XKF(3)        4    144
348000931008     O**VOLUME
348100970203     O                       XKI(3)        4    165
348200931008     O**SPEDIZIONI
348300970203     O                       XKL(3)        Z    174
348400940323     O*
348500090914     O****4°RIGA DEL DETTAGLIO TRANSITI = "C" - express
348600940323     O**COLLI
348700940323     O          E            DETTRA      1
348800090605     O**!!!                                      11 'CORR'
348900090605     o                       wdtsp(01)           11
349000970203     O                       XKA(1)        Z     18
349100940323     O**PESO
349200970203     O                       XKD(1)        4    124
349300970203     O                       XKE(1)        4    134
349400970203     O                       XKF(1)        4    144
349500940323     O**VOLUME
349600970203     O                       XKI(1)        4    165
349700940323     O**SPEDIZIONI
349800970203     O                       XKL(1)        Z    174
349900941012      * ------------------?    TOTALI  GENERALI    ?------------------
350000940401     O*TRATTINI DI SEPARAZIONE TRA I TRANSITI ED IL TOTALE GENERALE.
350100941012     O          E            DETTOT      1
350200941012     O                       SK(5)               66
350300941012     O                       SK(5)              132
350400970306     O                       SK(18)             198
350500931008     O****1°RIGA DEL DETTAGLIO TOT.GEN. = TOTALI
350600931008     O**COLLI
350700970203     O          E            DETTOT      1
350800970203     O                                           10 'TOT.GENER.'
350900970203     O                       TKA123        Z     18
351000931008     O*COLLI NO SPUNT. IN PART.
351100970203     O                       TGP88T        Z     58
351200970203     O                       TGC88T        4     65
351300970203     O                       PER3                66
351400950622     O*COLLI PISTOLA 87 (EX 88 E 89)
351500970203     O                       TGP87T        Z    106
351600970203     O                       TGC87T        4    113
351700970203     O                       PER34              114
351800931008     O**PESO
351900970203     O                       TKD123        4    124
352000970203     O                       TKE123        4    134
352100970203     O                       TKF123        4    144
352200931008     O**PESO TASSABILE
352300970203     O                       TGPETS        4    155
352400931008     O**VOLUME
352500970203     O                       TKI123        4    165
352600931008     O**SPEDIZIONI
352700970203     O                       TKL123        Z    174
352800931008     O*
352900090914     O****  RIGA DEL DETTAGLIO TOT.GEN. = "H"-h1030
353000090914     O**COLLI
353100090914     O          E    10      DETTOT      1
353200090914     o                       wdtsp(04)           11
353300090914     O                       TKA(4)        Z     18
353400090914     O**PESO
353500090914     O                       TKD(4)        4    124
353600090914     O                       TKE(4)        4    134
353700090914     O                       TKF(4)        4    144
353800090914     O**VOLUME
353900090914     O                       TKI(4)        4    165
354000090914     O**SPEDIZIONI
354100090914     O                       TKL(4)        Z    174
354200090914     O*
354300090914     O****2°RIGA DEL DETTAGLIO TOT.GEN. = "E"-priority
354400931008     O**COLLI
354500931008     O          E            DETTOT      1
354600090605     O**!!!                                      11 'ESPR'
354700090605     o                       wdtsp(02)           11
354800970203     O                       TKA(2)        Z     18
354900931008     O**PESO
355000970203     O                       TKD(2)        4    124
355100970203     O                       TKE(2)        4    134
355200970203     O                       TKF(2)        4    144
355300931008     O**VOLUME
355400970203     O                       TKI(2)        4    165
355500931008     O**SPEDIZIONI
355600970203     O                       TKL(2)        Z    174
355700931008     O*
355800950111     O****3°RIGA DEL DETTAGLIO TOT.GEN. = DISTRIBUZIONE
355900931008     O**COLLI
356000931008     O          E            DETTOT      1
356100090605     O**!!!                                      11 'DIST'
356200090605     o                       wdtsp(03)           11
356300970203     O                       TKA(3)        Z     18
356400931008     O**PESO
356500970203     O                       TKD(3)        4    124
356600970203     O                       TKE(3)        4    134
356700970203     O                       TKF(3)        4    144
356800931008     O**VOLUME
356900970203     O                       TKI(3)        4    165
357000931008     O**SPEDIZIONI
357100970203     O                       TKL(3)        Z    174
357200940323     O*
357300950111     O****4°RIGA DEL DETTAGLIO TOT.GEN. = CORRIERE
357400940323     O**COLLI
357500940323     O          E            DETTOT      1
357600090605     O**!!!                                      11 'CORR'
357700090605     o                       wdtsp(01)           11
357800970203     O                       TKA(1)        Z     18
357900940323     O**PESO
358000970203     O                       TKD(1)        4    124
358100970203     O                       TKE(1)        4    134
358200970203     O                       TKF(1)        4    144
358300940323     O**VOLUME
358400970203     O                       TKI(1)        4    165
358500940323     O**SPEDIZIONI
358600970203     O                       TKL(1)        Z    174
358700931008     O*TRATTINI DI SEPARAZIONE DOPO IL TOTALE GENERALE.
358800931008     O          E            DETTOT      1  1
358900931008     O                       SK(5)               66
359000931008     O                       SK(5)              132
359100970306     O                       SK(18)             198
359200931130      *
359300941012     O          E            FINSTA           63
359400931130     O                       SK1(1)              66
359500941012     O          E            FINSTA           64
359600931130     O                       SK1(2)              66
359700931130     O                       SK1(3)             132
359800931130     O                       SK1(4)             198
359900950111     O                       §QTKPM        2     72
360000931130     O                       §3IARR        Z     99
360100941012     O          E            FINSTA           65
360200941012     O                       SK1(7)              66
360300940126      * AVVISO DI FOGLI VIAGGIO APERTI NEL PERIODO
360400941012     O          E            FVAPER      2
360500941012     O                       SK1(5)             115
360600941012     O                       SK1(6)            +  0
360700931008     O****************************************************************
360800931130      *            2         3         4         5         6         7         8
360900940406**   CMD           0123456789012345678901234567890123456789012345678901234567890
361000950209OVRPRTF FILE(PRTF198) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.part.
361100950209')
361200930928**   SK
361300050802XXXXXXXXXXXXXXXXXXXX                                                 1
361400950111   STATISTICA PARTENZE - PARTITO  IL XX/XX/XXXX                      2
361500950111   STATISTICA PARTENZE - PARTITO DAL XX/XX/XXXX  AL XX/XX/XXXX       3
361600970807                      FNLSC1R                             PAG.XXXX   4
361700930928------------------------------------------------------------------   5
361800970502  PUNTO     COLLI  |--- COLLI NON SPUNTATI (PIST.88,89,98,96) ---|
361900970204OPERATIVO            DiskB  % sul    Altri  % sul   Totale  % sul    7
362000970204                           partito         partito         partito   8
362100970131 |----- S P U N T E   P I S T O L A   87 ------| |----- P   E   S    9
362200970204   DiskB  % sul    Altri  % sul   Totale  % sul   FINO 35 OLTRE 35  10
362300970204         partito         partito         partito                    11
362400970306  O -------|     PESO      VOLUME    SPEDIZIONI                     12
362500970305 Q    TOTALE  TASSABILE            Totale  Tot.Da                   13
362600970305                                   Partito Partire                  14
3627009701311234567890 1234567 1234567 123,45% 1234567 123,45% 1234567 123,45%  15
362800970131 1234567 123,45% 1234567 123,45% 1234567 123,45% 1234567,8 1234567  16
362900970305,8 1234567,8 12345678,9 12345,6789 1234567 1234567 1234567 123,45%  17
363000970306--------------------------------------------------                  18
363100931130**
363200940405N.B.
363300940405PESO TASSABILE:  E' CALCOLATO CON UN RAPPORTO PESO/VOLUME  1 MC =
363400940405XX,XXX Q.LI ARROTONDATO AI XXXXXX KG. SUPERIORI, PER LE SOLE SPEDI
363500940405ZIONI CON PESO FINO A KG. 3500, ESCLUSE BOLLE DI RECUPERO E C/SERV
363600940126***   SONO STATI TROVATI ALCUNI FOGLI VIAGGIO APERTI AVENTI DATA P
363700940126ARTENZA COMPRESA NEL PERIODO   ***
363800940406SPEDIZIONI:  DALLA STATISTICA SONO ESLUSE LE BOLLE DI RECUPERO
