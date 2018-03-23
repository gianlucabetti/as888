000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941025     H* TRTR91R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400941025     H*  RICEZIONE IN SEDE DATI CONSEGNA   RLS 11/94                 *
000500000000     H*--------------------------------------------------------------*
000600010103     FFIARB46R  UF   E             DISK    USROPN
000700941025     FTNBLA01L  IF   E           K DISK
000800990729     FTITAS30C  UF   E           K DISK
000900900604     FCMRIC01L  UF   E           K DISK
001000930517     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100900604     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001200900604     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001300900604     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001400900604     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
001500000000     D KPJBA         E DS
001600900604     D  LIBSYS                92    101
001700900604     D  LEGFIL               483    492
001800900604     D  MBRFIL               493    502
001900000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002000000000     D  TCU                  398    697
002100000000     D                                     DIM(50)                              TAB FLAG CNT
002200000000     D  KCU                  698    847P 0
002300000000     D                                     DIM(50)                              TAB CAPO CONTO
002400000000     D                                     PACKEVEN
002500000000     D  DTU                  848    895P 0
002600000000     D                                     DIM(12)                              DATE PARAM
002700000000     D                                     PACKEVEN
002800941025     D WLBDA8          DS
002900941025     D  G08DAT                 1      8  0
003000941025     D  G08INV                 9     16  0
003100941025     D  G08ERR                17     17
003200941025     D  G08TGI                18     22  0
003300000201     D TRUL52        E DS                  EXTNAME(TRUL52DS)
003400941025     D                SDS
003500941025     D  §PGM                   1     10
003600011221     D Data8           S               D   datfmt(*ISO)
003700011221     D UData           S               D   datfmt(*ISO)
003800040430     D Data30          S               D   datfmt(*ISO)
003900990729     ITITAS000      10
004000990729     ITITAS010      11
004100990729     ITITASP00      12
004200941025     I/SPACE 3
004300900604     C*---------------------------------------------------------------*
004400900604     C* APERTURA MEMBRO DA TRASCODIFICARE
004500930120     C*---------------------------------------------------------------*
004600900604     C                   EXSR      CAEXA
004700000000     C*---------------------------------------------------------------*
004800900604     C* LETTURA
004900930120     C*---------------------------------------------------------------*
005000941025     C     *IN01         DOUEQ     '1'
005100010103     C                   READ      FIARB46R                               01
005200911016     C*
005300941025     C     *IN01         IFEQ      *OFF
005400941025     C                   SETOFF                                       2125
005500941207     C                   SETOFF                                       101112
005600990729     C     KNSP          CHAIN     TITAS30C                           21
005700941207     C   21KNSP          CHAIN     TNBLA01L                           25
005800040430      **
005900040430      ** controllo se non presente la bolla nell'arco di 30 gg. allora il record
006000040430      ** spia dovrà essere eliminato
006100040430     c                   if        *in21 and *in25
006200040430      **
006300040430      **  x i ripristini va presa in considerazione la data immissione
006400040430     c                   if        arbDCM > 0 and arbfag <> 'R' or
006500040430     c                             arbDTI > 0 and arbfag  = 'R'
006600040430      **
006700040430      ** imposta la data x il confronto
006800040430     c                   if        arbFAG = 'R'
006900040430     c                   move      arbdti        data30
007000040430     c                   else
007100040430     c                   move      arbdcm        data30
007200040430     c                   end
007300040430      ** se + old di 30 gg.
007400040430     c                   adddur    30:*days      data30
007500040430     c                   if        data30 < udata
007600040430     C                   add       1             nurec
007700040430     C                   Delete    FIARBR00
007800040430     c                   end
007900040430      **
008000040430     c                   end
008100040430     c                   endIF
008200040430      **
008300000201     C* STAMPO L'ANOMALIA SOLO SE LA DATA CONSEGNA DELLA BOLLA E'SUPERIORE
008400000201     C* ALLA DATA DEL GIORNO - 2GG OPPURE SE E' DELL'ANNO PRECEDENTE
008500000201     C     ARBDCM        IFGT      0
008600000201     C     ARBDCM        ANDLT     D52DFI
008700000201     C     ARBAAS        ANDGT     WANNO
008800000201     C     ARBDCP        ORGT      0
008900000201     C     ARBDCM        ANDEQ     0
009000000201     C     ARBDCP        ANDLT     D52DFI
009100000201     C     ARBAAS        ANDGT     WANNO
009200000201     C*
009300930517     C* STAMPO ANOMALIA SE NON TROVO BOLLE
009400941207     C   21
009500941207     CAN 25              DO
009600930517     C  N99
009700930517     COR OF              EXCEPT    TESTA
009800930517     C  N99              SETON                                        99
009900930517     C                   EXCEPT    DETTAB
010000930517     C                   END
010100000201     C                   END
010200911115     C*
010300941207     C  N21              DO
010400941025     C                   EXSR      TAS
010500010806      * DELETO RECORD RICEVUTO SOLO SE NON ERA UN RIPRISTINO OPPURE
010600010806      *        ERA UN RIPRISTINO E SONO RIUSCITO AD EFFETTUARLO
010700011221      * oppure ERA UN RIPRISTINO E non SONO RIUSCITO AD EFFETTUARLO ma
010800011221      *        sono trascorsi più di 7 giorni.
010900010806     C     ARBFAG        IFNE      'R'
011000010806     C     ARBFAG        OREQ      'R'
011100010806     C     SIRIPR        ANDEQ     'S'
011200011221     C     ARBFAG        OREQ      'R'
011300011221     C     SIRIPR        ANDEQ     'N'
011400011221     C     OLDREC        ANDEQ     'S'
011500911115     C                   ADD       1             NUREC             7 0
011600010103     C                   DELETE    FIARBR00
011700010806     C                   ENDIF
011800010806      *
011900911115     C                   END
012000911115     C*
012100941025     C                   ENDIF
012200941025     C                   ENDDO
012300900604     C*
012400900604     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
012500900604     C*
012600900604     C                   EXSR      CAEXB
012700911018     C*
012800900604     C* AGGIORNA ARCHIVIO DI LOG
012900900604     C*
013000900604     C     LEGFIL        CHAIN     CMRIC01L                           20
013100900604     C  N20              DO
013200900604     C                   Z-ADD     NUREC         RICNR1
013300900604     C                   MOVE      '1'           RICFL2                          AGG.TO
013400900604     C                   UPDATE    CMRIC000
013500900604     C                   END
013600900604     C     STOP          TAG
013700000000     C                   SETON                                        LR
013800911115     C/SPACE
013900911115     C**---------------------------------------------------------
014000911115     C** MUOVE CAMPI BLTAS
014100911115     C**---------------------------------------------------------
014200911115     C     MOVCAM        BEGSR
014300010806     C                   MOVEL     'N'           SIRIPR            1            S/N RIPRISTINO
014400011221     C                   MOVEL     'N'           OLDREC            1            S/N VECCHIO RECORD
014500950412     C*  SE ARBFAG = 'C' AGGIORNO SOLO I DATI DI CONFORMITA'
014600950412     C                   MOVE      ARBNPI        TASNPI                         NON CONF.PART.INTER
014700950412     C                   MOVE      ARBNCI        TASNCI                         NON CONF.CONS.INTER
014800950412     C                   MOVE      ARBNRC        TASNRC                         NON CONF.RESA CL.
014900990729     C* DATA CONSEGNA PREVISTA
015000990729     C                   MOVE      ARBDCE        TASDCE                         DATA CONS.PREVISTA
015100950412     C     ARBFAG        IFNE      'C'
015200941025      * AGGIORNO  TUTTI I CAMPI ECCETTO DATA CONSEGNA
015300950421     C     ARBFAG        IFNE      'O'
015400040416     C                   if        arbfag = 'R' and tascca <> *blank
015500040416      * attenzione se c'è di mezzo un ripristino e CCA <> *blank
015600040416      * non deve aggiornare il campo altrimenti perde l'informazione
015700040416      * x esempio di bolle dirottate....
015800040416     c                   else
015900040416      * aggiorna solo se NON è un RIPRISTINO e non è una bolla ORIGINALE
016000941025     C                   MOVE      ARBCCA        TASCCA                         CONS. ANOMALA
016100040416     c                   end
016200950421     C                   END
016300010301      * AGGIORNA I DATI DI DISTINTA SOLO SE NON È STATO UN RIPRISTINO
016400010301     C     ARBFAG        IFNE      'R'
016500950412     C                   Z-ADD     ARBNDC        TASNDC                         NR.DIST.CONS.
016600010103     C                   Z-ADD     ARBPDC        TASPDC                         NR.DIST.CONS.
016700990729     C                   Z-ADD     ARBDDC        TASDDC                         DATA.DIST.CONS.
016800010301     C                   END
016900950412      * AGGIORNO DATA CONSEGNA SE NON è B.ORIGIN.DIROTTATA
017000010806      *                       E NON È UN RIPRISTINO
017100950412     C     ARBFAG        IFNE      'D'
017200010806     C     ARBFAG        ANDNE     'R'
017300950412     C                   Z-ADD     ARBDCM        TASDCM                         DATA CONSEGNA
017400950412     C                   Z-ADD     ARBHMC        TASHMC                         ORA CONSEGNA
017500010806     C                   ENDIF
017600010305      * SOLO X RIPRISTINO
017700010305      *  E SE LE DISTINTE COINICIDONO.
017800010305     C     ARBFAG        IFEQ      'R'
017900010305     C     ARBNDC        ANDEQ     TASNDC
018000010806     C                   Z-ADD     0             TASDCM                         DATA CONSEGNA
018100010806     C                   Z-ADD     0             TASHMC                         ORA CONSEGNA
018200010305     C                   Z-ADD     0             TASNDC                         NR.DIST.CONS.
018300010305     C                   Z-ADD     0             TASPDC                         NR.DIST.CONS.
018400010305     C                   Z-ADD     0             TASDDC                         DATA.DIST.CONS.
018500010806     C                   MOVEL     'S'           SIRIPR                         FATTO RIPRISTINO
018600010305     C                   END
018700011221      *
018800011221      *--------
018900011221      * SOLO X RIPRISTINO E SE LE DISTINTE NON COINCIDONO
019000011221      *
019100011221      *   E SOPRATTUTTO SONO PASSATI PIÙ GIORNI DEL DOVUTO
019200011221      * IMPOSTO IL RECORD PER LA CANCELLAZIONE
019300011221     C     ARBFAG        IFEQ      'R'
019400011221     C     ARBNDC        ANDNE     TASNDC
019500011221      *
019600011221      * Si è utilizzata la DATA arrivo trazione COME data di IMMISSIONE
019700011221      *  del RECORD di richiesta RIPRISTINO dal pgm FNLRT1R.
019800011221      *   QUI OCCORRE CONTROLLARE SE SONO PASSATI 7 GG. E QUINDI CANCELLARE
019900011221      * COMUNQUE IL RECORD ANCHE SE NON SI È GENERATO IL RIPRISTINO.
020000011221      *       >> ARBDTI qui e solo qui ha un diverso significato <<
020100011221     C     ARBDTI        ifne      0
020200011221     C                   move      ARBDTI        DATA8
020300011221     C                   ADDDUR    7:*days       DATA8
020400011221     C                   ELSE
020500011221      * SE ARBDTI = 0 FORZO UDATE ... CANCELLERO' IL GG SUCCESSIVO
020600011221     C                   MOVE      UDATA         DATA8
020700011221     C                   endif
020800011221      *                                                                         Ripristino non fatto
020900011221     C     DATA8         IfLT      Udata                                        ma
021000011221     C                   MOVEL     'S'           OLDREC                         da cancellare SI
021100011221     C                   END
021200011221      *
021300011221     C                   Endif
021400011221      *--------
021500011221      *
021600950412     C* AGGIORNO ALTRI CAMPI SOLO SE NON HO RICEVUTO RIPRISTINI
021700950412     C     ARBFAG        IFNE      'R'
021701160511     c* è stato sfruttato il campo ARBFLP (obsoleto, conteneva l'ultimo
021702160511     c* transito) per ricevere in sede la zona di consegna nel caso fosse
021703160511     c* variata
021800950427     C     ARBFAG        IFNE      'O'
022500160511     C                   z-add     ARBFLP        TASznc
022801160511     C                   END
022900990729     C                   MOVE      ARBDUC        TASDUC
022901160511     c*
023000030120     c                   If        TasDti = *Zeros
023300030225     c                   Z-Add     ArbDam        TASDTI                         DATA ARR.merce
023400030120     c                   EndIf
023500990729     C                   Z-ADD     ARBDCP        TASDCP                         DATA CON.PAR
023600950412     C                   END
023700950412     C*
023800950412     C                   END
023900950412     C*
024000911115     C                   ENDSR
024100911115     C/SPACE
024200911115     C**---------------------------------------------------------
024300990729     C** AGGIORNA TITAS00F-10F-P0F
024400911115     C**---------------------------------------------------------
024500911115     C     TAS           BEGSR
024600941207      *
024700941207     C     *IN21         DOWEQ     *OFF
024800911115     C                   EXSR      MOVCAM
024900990729     C* AGGIORNO TITAS000
025000941025     C   10              EXCEPT    AGGTAS
025100990729     C* AGGIORNO TITAS010
025200941025     C   11              EXCEPT    AGGTA1
025300990729     C* AGGIORNO TITASP00
025400941025     C   12              EXCEPT    AGGTAP
025500941025     C                   SETOFF                                       101112
025600990729     C     KNSP          READE     TITAS30C                               21
025700941025     C                   ENDDO
025800911115     C                   ENDSR
025900900604     C/SPACE
026000900604     C**---------------------------------------------------------
026100010103     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI FIARB46R
026200900604     C**---------------------------------------------------------
026300900604     C     CAEXA         BEGSR
026400900604     C                   MOVE      ')'           VAR11            11
026500900604     C                   MOVEL     MBRFIL        VAR11
026600900604     C                   MOVE      '))'          VAR12            12
026700900604     C                   MOVEL     MBRFIL        VAR12
026800900604     C                   Z-ADD     55            LUNG             15 5
026900900604     C*
027000900604     C                   SETOFF                                       20
027100911111     C                   MOVE      VAR11         CMD1
027200900604     C                   MOVEL     *BLANKS       COMMAN
027300911111     C                   MOVEA     CMD1(1)       COMMAN           80
027400900604     C                   CALL      'QCMDEXC'                            20
027500900604     C                   PARM                    COMMAN
027600900604     C                   PARM                    LUNG
027700900604     C   20              GOTO      STOP
027800900604     C** CONTROLLO SE ESISTE IL MEMBRO
027900900604     C** SE NON ESISTE CHIUDO IL PROGRAMMA
028000900604     C                   SETOFF                                       20
028100041008     c                   if        esegueALC = 'S'
028200911111     C                   MOVE      VAR12         CMD2
028300900604     C                   MOVEL     *BLANKS       COMMAN
028400911111     C                   MOVEA     CMD2(1)       COMMAN
028500900604     C                   CALL      'QCMDEXC'                            20
028600900604     C                   PARM                    COMMAN
028700900604     C                   PARM                    LUNG
028800900604     C   20              GOTO      STOP
028900041008     c                   end
029000900604     C** ALLOCO MEMBRO
029100900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
029200900604     C                   SETOFF                                       20
029300911111     C                   MOVE      VAR11         CMD3
029400900604     C                   MOVEL     *BLANKS       COMMAN
029500911111     C                   MOVEA     CMD3(1)       COMMAN
029600900604     C                   CALL      'QCMDEXC'                            20
029700900604     C                   PARM                    COMMAN
029800900604     C                   PARM                    LUNG
029900900604     C   20              GOTO      STOP
030000900604     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
030100900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
030200010103     C                   OPEN      FIARB46R
030300900604     C                   ENDSR
030400900604     C/SPACE 005
030500911018     C**------------------------------------------------------------
030600010103     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI FIARB46R
030700911018     C**------------------------------------------------------------
030800900604     C     CAEXB         BEGSR
030900900604     C                   MOVE      ')'           VAR11            11
031000900604     C                   MOVEL     MBRFIL        VAR11
031100900604     C                   MOVE      '))'          VAR12            12
031200900604     C                   MOVEL     MBRFIL        VAR12
031300900604     C                   Z-ADD     55            LUNG             15 5
031400900604     C*
031500010103     C                   CLOSE     FIARB46R
031600900627     C*
031700041008     c                   if        esegueALC = 'S'
031800911111     C                   MOVE      VAR12         CMD5
031900900604     C                   MOVEL     *BLANKS       COMMAN
032000911111     C                   MOVEA     CMD5(1)       COMMAN
032100900604     C                   CALL      'QCMDEXC'
032200900604     C                   PARM                    COMMAN
032300900604     C                   PARM                    LUNG
032400041008     c                   end
032500041008      *
032600900604     C** DISALLOCA IL MEMBRO AGG.TO
032700900604     C                   ENDSR
032800941025     C*---------------------------------------------------------------*
032900941025     C     *INZSR        BEGSR
033000000201     C*
033100941025     C     *ENTRY        PLIST
033200941025     C                   PARM                    KPJBA
033300941025     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
033400941025     C* ASSUMO IL PRIMO MEMBRO DEL FILE
033500941025     C     MBRFIL        IFEQ      *BLANK
033600941025     C                   MOVEL     'M888  '      MBRFIL
033700941025     C                   END
033800941025     C                   Z-ADD     1             CODUT
033900941025     C                   CALL      'XPARUT'
034000941025     C                   PARM                    UTEDSE
034100941025     C     CODUT         CABEQ     -1            STOP
034200941025     C                   MOVEL     RAGUT         RSUT             20
034300941025     C*§§
034400941025     C                   TIME                    WHHUDT           14 0
034500941025     C                   MOVEL     WHHUDT        ORA               6 0
034600941025     C                   MOVE      WHHUDT        G08DAT
034700941025     C                   MOVE      *ZEROS        G08INV
034800941025     C                   MOVEL     *BLANK        G08ERR
034900941025     C                   CALL      'XSRDA8'
035000941025     C                   PARM                    WLBDA8
035100941025     C                   Z-ADD     G08INV        AMGUDT            8 0
035200011221     C                   move      G08INV        Udata
035300011221     C                   move      G08INV        Data8
035400941025     C                   Z-ADD     G08DAT        GMAUDT            8 0
035500000201     C* CALCOLO LA DATA DI OGGI MENO 2 GG
035600000201     C                   CLEAR                   TRUL52
035700000201     C                   Z-ADD     AMGUDT        D52DIN
035800000201     C                   Z-ADD     2             D52GIO
035900000201     C                   MOVEL     '-'           D52SGN
036000000201     C                   CALL      'TRUL52R'
036100000201     C                   PARM                    TRUL52
036200000201     C* CALCOLO L'ANNO LIMITE PER NON STAMPARE REGISTR.
036300000201     C                   MOVEL     AMGUDT        WANNO             4 0
036400000201     C                   MOVE      AMGUDT        WMMGG             4 0
036500000201     C     WMMGG         IFLT      0115
036600000201     C                   SUB       2             WANNO
036700000201     C                   ELSE
036800000201     C                   SUB       1             WANNO
036900000201     C                   END
037000941025     C*
037100941025     C* DEFINIZIONE CHIAVI
037200941025     C*
037300941025     C     KNSP          KLIST
037400941025     C                   KFLD                    ARBAAS
037500941025     C                   KFLD                    ARBLNP
037600941025     C                   KFLD                    ARBNRS
037700941025     C                   KFLD                    ARBNSP
037800041008     C*
037900041008     C* esegue/non esegue allocazioni
038000041008     c                   move      'N'           esegueALC         1
038100041008     C*
038200941025     C                   ENDSR
038300990729     OTITAS000  E            AGGTAS
038400941025     O                       TASCCA
038500990729     O                       TASFL2
038600990729     O                       TASDUC
038700990729     O                       TASDTI
038800990729     O                       TASHTI
038900990729     O                       TASDCM
039000911115     O                       TASHMC
039100941025     O                       TASNDC
039200990729     O                       TASDDC
039300990729     O                       TASDCP
039400941025     O                       TASNPI
039500941025     O                       TASNCI
039600941025     O                       TASNRC
039700990729     O                       TASDCE
039800010104     O                       TASPDC
039900990729     OTITAS010  E            AGGTA1
040000941025     O                       TASCCA
040100990729     O                       TASFL2
040200990729     O                       TASDUC
040300990729     O                       TASDTI
040400990729     O                       TASHTI
040500941025     O                       TASDCM
040600941025     O                       TASHMC
040700941025     O                       TASNDC
040800990729     O                       TASDDC
040900990729     O                       TASDCP
041000941025     O                       TASNPI
041100941025     O                       TASNCI
041200941025     O                       TASNRC
041300990729     O                       TASDCE
041400010104     O                       TASPDC
041500990729     OTITASP00  E            AGGTAP
041600990729     O                       TASCCA
041700990729     O                       TASFL2
041800990729     O                       TASDUC
041900990729     O                       TASDTI
042000990729     O                       TASHTI
042100990729     O                       TASDCM
042200990729     O                       TASHMC
042300990729     O                       TASNDC
042400990729     O                       TASDDC
042500990729     O                       TASDCP
042600990729     O                       TASNPI
042700990729     O                       TASNCI
042800990729     O                       TASNRC
042900990729     O                       TASDCE
043000010104     O                       TASPDC
043100930517     OQSYSPRT   E            TESTA          1 01
043200930517     O                       RSUT              +  1
043300961022     O                                         +  1 'PUNTO OPERATIVO-'
043400930517     O                       MBRFIL            +  1
043500961022     O                                         +  3 '***'
043600930517     O                                         +  3 'RICEZIONE DATI CONSEGNA'
043700930517     O                                         +  3 '***'
043800941025     O                       §PGM               100
043900941025     O                       GMAUDT             115 '  /  /    '
044000941025     O                                          126 'PAG.'
044100930517     O                       PAGE          Z    132
044200941025     O          E            TESTA          1
044300941025     O                       KNSIF             +  0
044400941025     O                       KNMUS             +  1
044500941025     O                       ORA                115 '  :  :  '
044600930517     O          E            DETTAB      1
044700930517     O                       ARBAAS            +  1
044800930517     O                       ARBLNP            +  1
044900930517     O                       ARBNRS            +  1
045000930517     O                       ARBNSP            +  1
045100930517     O                                         +  1 'NON TROVATA BOLLA'
045200010103**         CMD1
045300010103CHKOBJ OBJ(FIARB46R) OBJTYPE(*FILE)     MBR(M234567890)
045400900604**         CMD2
045500010103ALCOBJ OBJ((FIARB46R *FILE *EXCL           M234567890))
045600900604**         CMD3
045700010103OVRDBF FILE(FIARB46R)                   MBR(M234567890)
045800900604**         CMD5
045900010103DLCOBJ OBJ((FIARB46R *FILE *EXCL           M234567890))
