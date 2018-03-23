000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*----------------------------------------------------------------H
000300890717     F*-------  TRAS03R   IMMISSIONE CONTRASSEGNI                 -----
000400000000     F*----------------------------------------------------------------H
000500000000     FTABEL00F  IF   E           K DISK
000600941028     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
000700990728     FTITAS30C  UF   E           K DISK
000800990728     FTITA430C  IF   E           K DISK
000900941028     FTNCSV00F  O    E           K DISK
001000941028     FTNCSB03L  UF A E           K DISK
001100941028     FTNCSA01L  O    E           K DISK
001200941028     FTNSC03D   CF   E             WORKSTN
001300941028     F*----------------------------------------------------------------H
001400011113     D                SDS
001500011113     D NomPgm            *PROC
001600140411     D SK              S             78    DIM(20) CTDATA PERRCD(1)
001700941028     D CCV             S              3    DIM(100)
001800941028     D FDC             S              1    DIM(100)
001900991207     D DEC             S              2  0 DIM(100)
002000121008     D D1A             S             78    DIM(200)
002100121008     D C1A             S              2    DIM(200)
002200020201     D com03           S              3    inz
002300941028     D*----------------------------------------------------------------
002400020130     D*  DS PER RICEZIONE PARAMETRI TIBS20R
002500020130     D TIBS20        E DS                  EXTNAME(TIBS20DS)
002600020130     D*  DS PER RICHIAMO TITAS
002700020130     D TNSB50        E DS                  EXTNAME(TNSB50DS)
002800941028     D CSBDS         E DS                  EXTNAME(TNCSB00F)
002900941028     D               E DS                  EXTNAME(TNCSV00F)
003000941028     D  CSVDS                 16    229
003100941028     D DSCV          E DS
003200000000     D PARAM           DS
003300891113     D  LINPAR                 1      3  0
003400891113     D  NUMSPE                 4     10  0
003500941028     D  VAAS                  11     14  0
003600941028     D  VNRS                  15     16  0
003700000000     D TCUDS           DS
003800000000     D  F3                     3      3
003900000000     D  F4                     4      4
004000000000     D  F56                    5      6
004100000000     D KPJBA         E DS
004200000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
004300000000     D  TCU                  398    697
004400000000     D                                     DIM(50)
004500000000     D  KCU                  698    847P 0
004600000000     D                                     DIM(50)
004700000000     D                                     PACKEVEN
004800991124     D DSBS02        E DS                  EXTNAME(TIBS02DS)
004900991124     D DGED          E DS
005000010222     D*FTRUL15    E DSTRUL15DS
005100011113     D Trul21DS      E DS
005200011113     D                                     INZ
005300971217     D TIBS69        E DS                  EXTNAME(TIBS69DS)
005400971217     D CNACO         E DS                  EXTNAME(CNACO00F)
005500971217     D CNIND         E DS                  EXTNAME(CNIND00F)
005600971217     D CNCLP         E DS                  EXTNAME(CNCLP00F)
005700971217     D FNCLS         E DS                  EXTNAME(FNCLS00F)
005800121008     D DSTM          E DS
005900121008     D DS1A          E DS
006000941028     D WLBDA8          DS
006100941028     D  G02DAT                 1      8  0
006200941028     D  G02INV                 9     16  0
006300941028     D  G02ERR                17     17
006400941028     D  G02TGI                18     22  0
006500020208     Dcom07            s              7    inz
006600000000     I*----------------------------------------------------------------
006700941028     IAZORG01L  AA
006800941028     I                             P    4    5 0ORGFIL
006900000120     I                                  7    7  ORGFAG
007000000120     I                                 14   33  ORGDES
007100941028     I                             P 4224 4225 0ORGFC5
007200941028     I*  Tipo Record TNTAS
007300990728     ITITAS000      01
007400990728     ITITAS010      02
007500990728     ITITASP00      03
007600000000     I/SPACE 3
007700000000     C*---------------------------------------------------------------*
007800020130     c* chiamato da pgm TIBS20R
007900121011     c                   if        i20car = '3' or i20car = '8'
008000020130     c                   exsr      srbolla
008100020130     c                   seton                                        04
008200020130     C                   ELSE
008300020130     c                   setoff                                       04
008400020130     C                   CLEAR                   tibs20
008500020130     c                   end
008600020130     c*
008700000000     C     VIDEO         TAG
008800020301     c* se sono chiamato da TIBS20R e c'è errore 1° video esco.
008900020301     c                   if        *in04 and *in28
009000020301     c                   eval      o20msg = $msg
009100020301     c                   goto      fine
009200020301     c                   end
009300941108     C                   SETOFF                                       15
009400941108     C                   MOVEL     *BLANKS       INDMIT
009500941108     C                   MOVEL     *BLANKS       CAPMIT
009600941108     C                   MOVEL     *BLANKS       LOCMIT
009700941108     C                   MOVEL     *BLANKS       DESMIT
009800941108     C                   MOVEL     *BLANKS       PROMIT
009900941108     C                   MOVEL     *BLANKS       NAZMIT
010000941108     C                   Z-ADD     0             DATSPE
010100941108     C                   MOVEL     *BLANKS       DESDES
010200941108     C                   MOVEL     *BLANKS       PRODES
010300941108     C                   MOVEL     *BLANKS       LOCDES
010400941108     C                   MOVEL     *BLANKS       RIFALF
010500011016     C                   MOVEL     §GEDCN        VALUTA
010600941108     C                   Z-ADD     0             RIFMIT
010700941108     C                   Z-ADD     0             IMPORT
010800020131     C* emette il primo video solo se non sono chiamato dal pgm TIBS20R
010900020131     c                   if        not *in04
011000941028     C                   EXFMT     SC03D01
011100020131     c                   end
011200941028     C                   SETOFF                                       284041
011300941028     C                   SETOFF                                       424344
011400971218     C                   SETOFF                                       48
011500941028     C   KC              GOTO      FINE
011600941028     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
011700941028     C                   MOVEL     VAAS          WAA2              2 0
011800941028     C     WAA2          IFEQ      0
011900941028     C     VAAS          IFGT      60
012000941028     C                   ADD       1900          VAAS
012100941028     C                   ELSE
012200941028     C                   ADD       2000          VAAS
012300941028     C                   END
012400941028     C                   END
012500941107     C   KG              GOTO      VISCAS
012600941107     C   KH              GOTO      VISBOL
012700000000     C*
012800900420     C* LINEA DI PARTENZA DEVE ESISTERE SU AZORG
012900941028     C                   Z-ADD     LINPAR        ORGFIL
013000941028     C     ORGFIL        CHAIN     AZORG01L                           21
013100941028     C     *IN21         IFEQ      '1'
013200941028     C                   SETON                                        2840
013300941028     C                   MOVEL     SK(3)         $MSG
013400941028     C                   GOTO      VIDEO
013500941028     C                   END
013600900420     C                   MOVEL     ORGDES        DESCAR
013700000000     C*
013800000000     C* TIPO BOLLA DEVE ESSERE CODIFICATO SU TABEL
013900000000     C                   MOVEL     'TB'          TBLCOD
014000000000     C                   MOVEL     *BLANK        TBLKEY
014100000000     C                   MOVEL     TIPBOL        TBLKEY
014200000000     C     CHITAB        SETLL     TABEL                                  21
014300941028     C     *IN21         IFEQ      '0'
014400941028     C                   SETON                                        2842
014500941028     C                   MOVEL     SK(5)         $MSG
014600941028     C                   GOTO      VIDEO
014700941028     C                   END
014800000000     C                   MOVEL     TIPBOL        BOLBOL            1
014900891113     C                   MOVE      CODCLI        SERV89            4 0
015000000000     C* CONTROLLO CONGRUENZA TIPO BOLLA - CODICE CLIENTE
015100020301     C     SERV89        IFEQ      9999
015200020301     C     BOLBOL        IFNE      'A'
015300020301     C                   SETON                                        284243
015400020301     C                   MOVEL     SK(6)         $MSG
015500020301     C                   GOTO      VIDEO
015600020301     C                   END
015700020301     C                   ELSE
015800020301     C     SERV89        ifeq      8888
015900020301     C     BOLBOL        andne     'F'
016000020301     C                   SETON                                        284243
016100020301     C                   MOVEL     SK(6)         $MSG
016200020301     C                   GOTO      VIDEO
016300020301     C                   END
016400020301     C                   END
016500000000     C*
016600000000     C* NUMERO DI SPEDIZIONE
016700941028     C     NUMSPE        IFEQ      0
016800941028     C                   SETON                                        2841
016900941028     C                   MOVEL     SK(4)         $MSG
017000941028     C                   GOTO      VIDEO
017100941028     C                   END
017200000000     C*
017300941028     C* SE CONTRASSEGNO ESISTE GIA SU TNCSB ERRORE
017400000000     C*
017500941028     C     CHICON        SETLL     TNCSB000                               21
017600941031     C     *IN21         IFEQ      '1'
017700941031     C                   SETON                                        28
017800941031     C                   MOVEL     SK(1)         $MSG
017900941031     C                   GOTO      VIDEO
018000941031     C                   END
018100000000     C*
018200000000     C* CODICE CLIENTE
018300000000     C*
018400941028     C     CODCLI        IFEQ      0
018500941028     C                   SETON                                        2843
018600941028     C                   MOVEL     SK(7)         $MSG
018700941028     C                   GOTO      VIDEO
018800941028     C                   END
018900941107      * CONTROLLO SE LA BOLLA ESISTE
019000941107      * SE ESISTE PRENDO I DATI DELLA BOLLA
019100941107      *
019200941107     C                   SETOFF                                       010203
019300990728     C     CHIBOL        CHAIN(N)  TITAS30C                           31
019400941107     C     *IN31         IFEQ      '0'
019500990728     C                   MOVE      TASRMN        RIFMIT
019600941107     C                   MOVE      TASLNA        LINARR
019700941107     C                   MOVEL     TASAAS        G02INV
019800941107     C                   MOVE      TASMGS        G02INV
019900941107     C                   MOVE      *ZEROS        G02DAT
020000941107     C                   MOVEL     '3'           G02ERR
020100941107     C                   CALL      'XSRDA8'
020200941107     C                   PARM                    WLBDA8
020300941107     C                   MOVE      G02DAT        DATSPE
020400990728     C                   MOVEL     TASRSD        DESDES
020500990728     C                   MOVEL     TASLOD        LOCDES
020600990728     C                   MOVEL     TASPRD        PRODES
020700021120     c                   if        prodes = *blanks
020800021120     c                   movel     tasnzd        prodes
020900021120     c                   end
021000990728     C* Reperisco riferimento mittente alfabetico
021100990728     C                   MOVEL     'A'           KTRC
021200990728     C                   MOVEL     *BLANKS       TA4NOT
021300990728     C     CHIBO4        CHAIN(N)  TITA430C                           34
021400990728     C                   MOVEL     TA4NOT        RIFALF
021500030115     c                   else
021600030115     C*
021700030115     C* CONTROLLO ANNO SPEDIZIONE CHE SIA CORRETTO solo se non trovo la bolla
021800030115     C                   MOVE      WHHDAT        UAAS              4 0
021900140411     C     VAAS          COMP      UAAS                               757454
022000140411     C   74              SUB       1             UAAS
022100140411     C   74VAAS          COMP      UAAS                               757454
022200140411     C
022300140411     C                   IF        *in74 and uaas2 =  0
022400140411     c                   z-add     uaas          uaas2             4 0
022500140411     C                   seton                                        2844
022600140411     C                   MOVEL     SK(9)         $MSG
022700140411     C                   GOTO      VIDEO
022800030115     C                   END
022900140411     C                   IF        *in75
023000140411     C                   seton                                        2844
023100140411     C                   MOVEL     SK(20)        $MSG
023200140411     C                   GOTO      VIDEO
023300140411     C                   END
023400140411     c                   if        vnrs <> 99 and uaas2 > 0
023500140320     C                   seton                                        2844
023600140320     C                   MOVEL     SK(9)         $MSG
023700140320     C                   GOTO      VIDEO
023800140411     c                   else
023900140411     c                   clear                   uaas2
024000140320     c                   endif
024100941107     C                   ENDIF
024200990728     C***             LINEA DI ARRIVO
024300971217     C                   Z-ADD     LINARR        ORGFIL
024400971217     C     ORGFIL        CHAIN     AZORG01L                           21
024500971217     C     *IN21         IFEQ      '1'
024600971218     C                   SETON                                        2848
024700971217     C                   MOVEL     SK(10)        $MSG
024800971217     C                   GOTO      VIDEO
024900971217     C                   END
025000971217     C                   MOVEL     ORGDES        DESARR
025100971217     C* CONTROLLO ESISTENZA RAGGRUPPAMENTO C/A  VOCE 125 (EX 220)
025200971217     C                   Z-ADD     LINARR        ORGFIL
025300971217     C     ORGFIL        CHAIN     AZORG01L                           84
025400971217     C  N84              Z-ADD     ORGFC5        CSBRGF
025500971217     C     *IN84         IFEQ      '1'
025600000120     C     ORGFAG        ORNE      'F'
025700000120     C     ORGFAG        ANDNE     'A'
025800971218     C                   SETON                                        2848
025900971217     C                   MOVEL     SK(10)        $MSG
026000971217     C                   GOTO      VIDEO
026100971217     C                   END
026200000000     C*
026300000000     C* SELEZIONE CODICE CLIENTE
026400000000     C*
026500891113     C     SERV89        IFNE      8888
026600941107     C     SERV89        ANDNE     9999
026700971217      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE
026800971217     C                   CLEAR                   TIBS69
026900971218     C                   CLEAR                   CNACO
027000971218     C                   CLEAR                   CNIND
027100971218     C                   CLEAR                   CNCLP
027200971218     C                   CLEAR                   FNCLS
027300971217     C                   MOVEL     ' '           I69TLA
027400971217     C                   MOVEL     KNSIF         I69SIF
027500971217     C                   Z-ADD     CODCLI        I69KAC
027600971217     C                   Z-ADD     CODCLI        I69KIN
027700971217     C                   CALL      'TIBS69R'
027800971217     C                   PARM                    TIBS69
027900971217     C                   PARM                    CNACO
028000971217     C                   PARM                    CNIND
028100971217     C                   PARM                    CNCLP
028200971217     C                   PARM                    FNCLS
028300971217     C     O69ERR        IFNE      *BLANK
028400971217     C                   SETON                                        2843
028500971217     C                   MOVEL     O69MSG        $MSG
028600971217     C                   GOTO      VIDEO
028700971217     C                   ENDIF
028800000000     C                   MOVEL     ACORAG        DESMIO
028900000000     C                   MOVEL     INDVIA        INDMIO
029000941028     C                   MOVEL     *BLANKS       CAPMIO
029100941028     C     INDCAP        IFNE      *ZEROS
029200941028     C                   MOVEL     INDCAP        CAPMIO
029300941028     C                   END
029400941028     C     INDCAE        IFNE      *BLANKS
029500941028     C     INDCAE        ANDNE     *ZEROS
029600941028     C                   MOVEL     INDCAE        CAPMIO
029700941028     C                   END
029800000000     C                   MOVEL     INDCIT        LOCMIO
029900941028     C                   MOVEL     INDPRV        PROMIO
030000941028     C                   MOVEL     INDSTA        NAZMIO
030100910930     C                   MOVE      ACOKSC        CDI
030200000000     C                   SETON                                        15
030300000000     C                   END
030400000000     C* EMISSIONE CARICAMENTO CONTRASSEGNO
030500000000     C*
030600000000     C     DISPLY        TAG
030700000000     C*
030800941028     C                   EXFMT     SC03D02
030900941028     C                   SETOFF                                       284041
031000941028     C                   SETOFF                                       424344
031100941028     C                   SETOFF                                       454647
031200121023     C                   SETOFF                                       4849
031300941028     C   KC              GOTO      FINE
031400020131     c* se chiamato da TIBS20R e premo f12 torno al pgm chiamante
031500020131     c                   if        *inkl
031600020201     c                   if        *in04
031700020131     c                   goto      fine
031800020131     c                   else
031900020131     C                   GOTO      VIDEO
032000020131     c                   end
032100020131     c                   end
032200000000     C*
032300900420     C* CONTROLLI:
032400000000     C*       DATA SPEDIZIONE
032500941028     C                   CLEAR                   WLBDA8
032600941028     C                   Z-ADD     DATSPE        G02DAT
032700941028     C                   CALL      'XSRDA8'
032800941028     C                   PARM                    WLBDA8
032900941028     C     G02ERR        IFEQ      '1'
033000941028     C                   SETON                                        2841
033100941028     C                   MOVEL     SK(11)        $MSG
033200941028     C                   GOTO      DISPLY
033300941028     C                   END
033400941028     C                   Z-ADD     G02DAT        DATSPE
033500891113     C* SE CLIENTE 8888/9999 VOCI MITTENTE OBBLIGATORIE
033600000000     C     *IN15         IFEQ      '0'
033700941028     C     DESMIT        IFEQ      *BLANKS
033800941028     C     INDMIT        OREQ      *BLANKS
033900941028     C     LOCMIT        OREQ      *BLANKS
034000941028     C     PROMIT        OREQ      *BLANKS
034100941028     C                   SETON                                        2842
034200941028     C                   MOVEL     SK(12)        $MSG
034300941028     C                   GOTO      DISPLY
034400000000     C                   END
034500941031     C                   END
034600000000     C* CAMPI DESTINATARIO OBBLIGATORI
034700941031     C     DESDES        IFEQ      *BLANKS
034800941031     C     PRODES        OREQ      *BLANKS
034900941031     C     LOCDES        OREQ      *BLANKS
035000941028     C                   SETON                                        2845
035100941028     C                   MOVEL     SK(15)        $MSG
035200941028     C                   GOTO      DISPLY
035300941028     C                   END
035400000000     C* RIFERIMENTI MITTENTE OBBLIGATORIO
035500941028     C     RIFMIT        IFEQ      0
035600941028     C                   SETON                                        2843
035700941028     C                   MOVEL     SK(13)        $MSG
035800941028     C                   GOTO      DISPLY
035900941028     C                   END
036000000000     C* IMPORTO CONTRASSEGNO
036100941028     C     IMPORT        IFEQ      0
036200941028     C                   SETON                                        2846
036300941028     C                   MOVEL     SK(16)        $MSG
036400941028     C                   GOTO      DISPLY
036500941028     C                   END
036600941028     C*
036700941028     C                   Z-ADD     1             XX                3 0
036800941028     C     VALUTA        LOOKUP    CCV(XX)                                31
036900941028     C     *IN31         IFEQ      '0'
037000941028     C                   SETON                                        4728
037100941028     C                   MOVEL     SK(17)        $MSG
037200941028     C                   ELSE
037300941028     C                   MOVE      IMPORT        COMO3             3 0
037400991207     C                   MOVE      IMPORT        COMO2             2 0
037500991207     C                   MOVE      IMPORT        COMO1             1 0
037600941028     C     COMO3         IFNE      *ZEROS
037700941028     C     FDC(XX)       ANDNE     'S'
037800941028     C     COMO3         ORNE      *ZEROS
037900991124     C     VALUTA        ANDEQ     *BLANKS
038000991207     C     COMO1         ORNE      *ZEROS
038100991207     C     DEC(XX)       ANDLT     3
038200991207     C     COMO2         ORNE      *ZEROS
038300991207     C     DEC(XX)       ANDLT     2
038400941028     C                   SETON                                        4728
038500941028     C                   MOVEL     SK(18)        $MSG
038600941028     C                   END
038700941028     C                   END
038800121008     C* tipo incasso contrassegno solo se causale = '3' C/A omesso
038900121011     c                   clear                   o20tinc
039000121008     C                   IF        tipoinc <> *blank
039100121008     C                   exsr      vertipoinc
039200121008     C                   if        *in49
039300121011     c                   clear                   o20tinc
039400121008     c                   seton                                        28
039500121008     C                   eval      $msg = 'Tipo incasso non valido'
039600121008     C                   GOTO      DISPLY
039700121011     c                   else
039800121011     c                   eval      o20tinc = tipoinc
039900121008     C                   Endif
040000121008     C                   END
040100121008     C*
040200011113     ** Controllo importo massimo contrassegno.
040300011113     C                   EXSR      ChkCasMax
040400011113     C                   IF        O21Fx2 <> *BLANK
040500011114     C                             OR
040600011114     C                             O21FMn <> *BLANK
040700011113     C                   SETON                                        2846
040800011113     C                   EVAL      $Msg =
040900011114     C                             'Immettere un importo compreso fra '
041000011114     C                             + %TRIM(%EDITC(O21LMn:'2')) + ' ' + §GEDMc
041100011114     C                             + ' e '
041200011113     C                             + %TRIM(%EDITC(O21Lx2:'2')) + ' ' + §GEDMc
041300011113     C                             + '.'
041400011113     C                   GOTO      Disply
041500011114     C                   ENDIF
041600000000     C* FINE CONTROLLI----------------------------------------------------
041700941028     C  NKF              GOTO      DISPLY
041800000000     C* REGISTRAZIONE CONTRASSEGNO
041900020131     C                   MOVE      046           CSBFLE
042000910912     C                   MOVEL     TIPBOL        CSBTBL
042100910912     C                   Z-ADD     LINPAR        CSBLNP
042200910912     C                   Z-ADD     NUMSPE        CSBNSP
042300941028     C                   Z-ADD     G02INV        CSBMGS
042400910912     C                   Z-ADD     CODCLI        CSBKSC
042500941028     C                   Z-ADD     IMPORT        CSBCAS
042600941108     C                   MOVEL     VALUTA        CSBVCA
042700941028     C                   MOVEL     *BLANKS       CSBNRA
042800910912     C                   Z-ADD     0             CSBDTE
042900910912     C                   Z-ADD     0             CSBBNA
043000910912     C                   Z-ADD     LINARR        CSBLNA
043100910912     C                   Z-ADD     0             CSBDTA
043200910912     C                   Z-ADD     0             CSBDDC
043300910912     C                   Z-ADD     0             CSBNDT
043400910912     C                   Z-ADD     STA           CSBSTA
043500910912     C                   MOVEL     *ZEROS        CSBABI
043600910912     C                   MOVEL     *ZEROS        CSBCAB
043700910912     C                   MOVEL     *BLANK        CSBTPA
043800910912     C                   MOVEL     *BLANK        CSBTPI
043900910912     C                   MOVEL     *BLANK        CSBFIN
044000910912     C                   MOVEL     *BLANK        CSBFPC
044100910912     C                   MOVEL     *ZEROS        CSBNDP
044200910912     C                   MOVEL     *ZEROS        CSBDDP
044300910912     C                   MOVEL     *ZEROS        CSBNRG
044400910912     C                   MOVEL     *ZEROS        CSBDRG
044500931108     C                   MOVEL     'R'           CSBFTR
044600931108     C                   MOVE      DATEU         CSBDTR
044700910912     C                   MOVEL     *ZEROS        CSBFGC
044800910912     C                   MOVEL     *ZEROS        CSBNGC
044900910912     C                   MOVEL     *ZEROS        CSBDGC
045000910912     C                   MOVEL     *BLANK        CSBMAS
045100910912     C                   MOVEL     *BLANK        CSBFUS
045200910913     C   15              Z-ADD     CDI           CSBCDI
045300910913     C  N15              Z-ADD     CSBKSC        CSBCDI
045400910912     C                   Z-ADD     RIFMIT        CSBRMN
045500941028     C                   MOVEL     RIFALF        CSBRMA
045600910912     C                   MOVEL     DESDES        CSBRSD
045700910912     C                   MOVEL     LOCDES        CSBPAR
045800910912     C                   MOVEL     PRODES        CSBPRO
045900941028     C                   MOVEL     G02INV        CSBAAS
046000910912     C                   MOVE      *ZEROS        CSBUDA
046100910912     C                   Z-ADD     VNRS          CSBNRS
046200000000     C*
046300941028     C                   WRITE     TNCSB000
046400941116     C*
046500941116     C*  Aggiorno Flag su TNTAS
046600941116     C                   SETOFF                                       010203
046700990728     C     CHIBOL        CHAIN     TITAS30C                           31
046800941116     C     *IN31         IFEQ      '0'
046900941116     C                   MOVEL     'S'           TASFCA
047000941116     C   01              EXCEPT    AGG00
047100941116     C   02              EXCEPT    AGG10
047200941116     C   03              EXCEPT    AGGP0
047300941116     C                   END
047400910912     C* SCRIVO STORICO
047500910912     C                   MOVE      046           CSVFEV
047600020201     c                   if        *in04 and %subst(knmus: 1: 3) = 'FIL'
047700020201     c                   eval      com03 =  %subst(knmus: 4: 3)
047800020201     c                   move      com03         csvfle
047900020131     c                   end
048000910912     C                   MOVEL     'IMCA'        CSVCAV
048100910912     C                   TIME                    CSVORV
048200910912     C                   MOVE      DATEU         CSVDTV
048300910912     C                   MOVEL     CSBDS         CSVDS
048400941028     C                   WRITE     TNCSV000
048500910912     C*
048600000000     C     *IN15         IFEQ      '0'
048700891113     C* CREO BLCAN00F SE CODICE CLIENTE 8888/9999
048800910912     C                   Z-ADD     LINPAR        CSALNP
048900910912     C                   Z-ADD     NUMSPE        CSANSP
049000910912     C                   MOVEL     DESMIT        CSARSD
049100910912     C                   MOVEL     INDMIT        CSAVID
049200941028     C                   MOVEL     CAPMIT        CSACAD
049300910912     C                   MOVEL     LOCMIT        CSACID
049400941028     C                   MOVEL     PROMIT        CSAPRM
049500941028     C                   MOVEL     NAZMIT        CSANZM
049600910912     C                   Z-ADD     VNRS          CSANRS
049700941028     C                   MOVEL     G02INV        CSAAAS
049800000000     C*
049900941028     C                   WRITE     TNCSA000
050000000000     C                   END
050100000000     C                   GOTO      FINE
050200000000     C     VISBOL        TAG
050300000000     C*
050400941104     C                   MOVEL     KPJBU         KPJBUS          256
050500941107     C                   MOVEL     *BLANK        KPJBU
050600990728     C                   CLEAR                   TNSB50
050700990728     C                   MOVEL     'C01'         I50OP0
050800990728     C                   MOVEL     TNSB50        KPJBU
050900990728     C                   CALL      'TNSB50R'
051000941104     C                   PARM                    KPJBA
051100990728     C                   MOVEL     KPJBU         TNSB50
051200990728     C                   MOVE      D50AAS        VAAS
051300990728     C                   MOVE      D50LNP        LINPAR
051400990728     C                   MOVE      D50NRS        VNRS
051500990728     C                   MOVE      D50NSP        NUMSPE
051600990728     C                   MOVE      D50TBL        TIPBOL
051700941104     C                   MOVEL     KPJBUS        KPJBU
051800000000     C                   GOTO      VIDEO
051900941107     C     VISCAS        TAG
052000941107     C*
052100941107     C                   MOVEL     KPJBU         KPJBUS          256
052200941107     C                   MOVEL     *BLANK        KPJBU
052300951211     C                   CALL      'TNSC40R'
052400941107     C                   PARM                    KPJBA
052500941107     C                   MOVEL     KPJBUS        KPJBU
052600941107     C                   GOTO      VIDEO
052700000000     C     FINE          TAG
052800020131     c*
052900941031     C  NKF              MOVEL     *ZEROS        PARAM
053000000000     C                   MOVEL     PARAM         KPJBU
053100020131     c*
053200020131     c                   if        *in04
053300020131     c                   if        not *inkf
053400020131     c                   movel     '1'           o20err
053500020131     c                   end
053600020131     c                   movel(p)  tibs20        kpjbu
053700020131     c                   end
053800020131     c*
053900000000     C                   Z-ADD     0             LINPAR
054000000000     C                   Z-ADD     0             NUMSPE
054100891113     C                   Z-ADD     0             VAAS
054200891113     C                   Z-ADD     0             VNRS
054300941028     C*
054400971218     C                   CLEAR                   TIBS69
054500971218     C                   MOVEL     'C'           I69TLA
054600971218     C                   CALL      'TIBS69R'
054700971218     C                   PARM                    TIBS69
054800971218     C                   PARM                    CNACO
054900971218     C                   PARM                    CNIND
055000971218     C                   PARM                    CNCLP
055100971218     C                   PARM                    FNCLS
055200910913     C                   SETON                                        LR
055300011113
055400011113     ** Controllo importo massimo contrassegno.
055500011113     C     ChkCasMax     BEGSR
055600011113     C*
055700011113     C                   CLEAR                   Trul21DS
055800011114     C*
055900011114     C                   IF        %FOUND(TiTas30C)
056000011114     C                   EVAL      I21TSp = TasTSp
056100011113     C                   EVAL      I21NzD = TasNzD
056200011113     C                   MOVE      TasCTr        I21CTr
056300011114     C                   ENDIF
056400011114     C*
056500011114     C                   EVAL      I21CBo = TipBol
056600011114     C                   EVAL      I21LnP = LinPar
056700011114     C                   EVAL      I21LnA = LinArr
056800011114     C                   EVAL      I21Ksc = CodCli
056900011114     C                   EVAL      I21NzM = NazMit
057000011114     C                   EVAL      I21TiC = *BLANK
057100011113     C                   EVAL      I21Imp = Import
057200011113     C                   EVAL      I21Div = Valuta
057300011113     C                   EVAL      I21Ute = KNmUs
057400011113     C                   EVAL      I21Pgm = NomPgm
057500011114     C*
057600011113     C                   CALL      'TRUL21R'
057700011113     C                   PARM                    Trul21DS
057800011113     C*
057900011113     C                   ENDSR
058000121008     C*---------------------------------------------------------------*
058100121008     C     vertipoinc    BEGSR
058200121008     C*---------------------------------------------------------------*
058300121008     C*  Controllo se richiesta ricerca tipo incasso
058400121023     C     '?'           SCAN      tipoinc                                16
058500121023     C     *IN16         IFEQ      '1'
058600121011     C                   MOVEL     KKUT          kKUT
058700121011     C                   MOVEL     '1A'          kCOD
058800121008     C                   MOVE      *BLANKS       tipoinc
058900121008     C                   CALL      'X§TABER'
059000121011     C                   PARM                    kKUT
059100121011     C                   PARM                    kCOD
059200121011     C                   PARM                    kkey
059300121011     C                   PARM                    §DES             30
059400121011     C                   MOVEL     kkey          tipoinc
059500121008     C                   END
059600121008     C                   Z-ADD     1             X
059700121008     C     tipoinc       LOOKUP    C1A(X)                                 95
059800121008     C     *IN95         IFEQ      '0'
059900121008     C                   SETON                                        49
060000121008     C                   GOTO      UPDRE1
060100121008     C                   endif
060200121008     C                   MOVEL     D1A(X)        DS1A
060300121008     C     tipoInc       IFEQ      'TM'
060400121008     C     tipoInc       OREQ      'TO'
060500121008     C     tipoInc       OREQ      'BB'
060600121008     C     tipoInc       OREQ      'BC'
060700121008     C     tipoInc       OREQ      'CB'
060800121008     C     tipoInc       OREQ      'BA'
060900121008     C     tipoInc       OREQ      'CA'
061000121008     C     tipoInc       OREQ      'BP'
061100121008     C                   CLEAR                   DSTM
061200130328     C                   MOVEL     'TM'          TBLCOD
061300130328     C                   MOVEL     *BLANK        TBLKEY
061400130328     C                   MOVEL     codcli        TBLKEY
061500130411     C     CHITAB        chain     TABEL                              21
061600130328     C     *IN21         IFEQ      '0'
061700130328     C                   movel     tbluni        DSTM
061800130328     c                   endif
061900121008     C*
062000121008     C     tipoinc       IFEQ      'TM'
062100121008     C     §TMFTM        ANDNE     'S'
062200121008     C     *IN95         ANDEQ     '1'
062300121008     C     tipoinc       OREQ      'BB'
062400121008     C     §TMFBB        ANDNE     'S'
062500121008     C     *IN95         ANDEQ     '1'
062600121008     C     tipoinc       OREQ      'TO'
062700121008     C     §TMFTO        ANDNE     'S'
062800121008     C     *IN95         ANDEQ     '1'
062900121008     C     tipoinc       OREQ      'BB'
063000121008     C     §TMFBC        ANDEQ     'S'
063100121008     C     *IN95         ANDEQ     '1'
063200121008     C     tipoinc       OREQ      'BC'
063300121008     C     §TMFBC        ANDNE     'S'
063400121008     C     *IN95         ANDEQ     '1'
063500121008     C     tipoinc       OREQ      'CB'
063600121008     C     §TMFBB        ANDNE     'S'
063700121008     C     *IN95         ANDEQ     '1'
063800121008     C     tipoinc       OREQ      'CB'
063900121008     C     §TMFBC        ANDEQ     'S'
064000121008     C     *IN95         ANDEQ     '1'
064100121008     C     tipoinc       OREQ      'BA'
064200121008     C     §TMFBA        ANDNE     'S'
064300121008     C     *IN95         ANDEQ     '1'
064400121008     C     tipoinc       OREQ      'CA'
064500121008     C     §TMFCA        ANDNE     'S'
064600121008     C     *IN95         ANDEQ     '1'
064700121008     C     tipoinc       OREQ      'BP'
064800121008     C     §TMFbp        ANDNE     'S'
064900121008     C     *IN95         ANDEQ     '1'
065000121008     C     *IN95         OREQ      '0'
065100121008     C                   SETON                                        49
065200121008     C                   GOTO      UPDRE1
065300121008     C                   END
065400121008     C                   END
065500121008     C*
065600121008     C     updre1        ENDSR
065700000000     C*---------------------------------------------------------------*
065800020130     C* AGGANCIO BOLLA                                                *
065900941028     C*---------------------------------------------------------------*
066000020130     C     SRBOLLA       BEGSR
066100020130     C*
066200020131     c     kbol20        chain     titas30c
066300020131     c                   if        %found(titas30c)
066400020130     c                   eval      linpar = i20lnp
066500020130     c                   eval      vnrs = i20nrs
066600020130     c                   eval      numspe = i20nsp
066700020131     c                   eval      tipbol = tastbl
066800020204     c                   eval      vaas = i20aas
066900020204     c                   eval      linarr = i20lna
067000020204     c                   eval      codcli = tasccm
067100020301     c                   move      tasccm        serv89
067200020301     c                   if        %subst(tastbl: 1: 1) = 'A' and
067300020301     c                             serv89 = 8888
067400020301     c                   move      linarr        com03             3
067500020301     c                   eval      com07 = com03 + '9999'
067600020301     c                   move      com07         codcli
067700020301     c                   end
067800020131     c* stato contrassegno
067900020131     C                   Z-ADD     3             STA
068000020131     c                   else
068100020201     c                   movel     '1'           o20err
068200020201     c                   movel(p)  tibs20        kpjbu
068300020131     c                   exsr      uscita
068400020131     c                   end
068500020130     C*
068600020130     C                   ENDSR
068700020131     ***********************************************************************
068800020131     ** Uscita dal programma.
068900020131     ***********************************************************************
069000020131     C     Uscita        BEGSR
069100020131     C*
069200020131     C                   EVAL      *INLR = *ON
069300020131     C                   RETURN
069400020131     C*
069500020131     C                   ENDSR
069600020130     C*---------------------------------------------------------------*
069700020130     C* OPERAZIONI INIZIALI                                           *
069800020130     C*---------------------------------------------------------------*
069900020130     C     *INZSR        BEGSR
070000020130     C*
070100941028     C     *ENTRY        PLIST
070200941028     C                   PARM                    KPJBA
070300020208     c                   if        kpjbu <> *blanks
070400020208     c                   movel     kpjbu         TIBS20
070500020208     c                   else
070600020208     c                   clear                   tibs20
070700020208     c                   end
070800941028     C                   Z-ADD     1             CODUT
070900941028     C                   CALL      'XPARUT'
071000941028     C                   PARM                    UTEDSE
071100941028     C     CODUT         CABEQ     -1            FINE
071200941028     C                   MOVEL     RAGUT         RSUT             20
071300941028     C                   MOVEL     RAGUT         RSUT2            25
071400941028     C*--- RICERCA CAPOCONTI------------------------------------------*
071500941028     C                   DO        50            X                 2 0
071600941028     C                   MOVE      TCU(X)        TCUDS
071700941031     C     F56           IFEQ      'CG'
071800941028     C     F4            ANDEQ     '1'
071900941031     C     F3            ANDEQ     '0'
072000941028     C                   Z-ADD     KCU(X)        KCI               4 0
072100941028     C                   END
072200941028     C                   END
072300941028     C*---------------------------------------------------------------*
072400941028     C* GIRO UDATE
072500941028     C                   CLEAR                   WLBDA8
072600941028     C                   TIME                    WHHDAT           14 0
072700941028     C                   MOVE      WHHDAT        G02DAT
072800941028     C                   CALL      'XSRDA8'
072900941028     C                   PARM                    WLBDA8
073000941028     C                   Z-ADD     G02INV        DATEU             8 0
073100941028     C*
073200941028     C* DEFINIZIONE CHIAVI
073300941028     C*
073400941028     C     CHITAB        KLIST
073500941028     C                   KFLD                    CODUT
073600941028     C                   KFLD                    TBLCOD
073700941028     C                   KFLD                    TBLKEY
073800941028     C*
073900020131     C     kbol20        KLIST
074000020131     C                   KFLD                    i20aas
074100020131     C                   KFLD                    i20lnp
074200020131     C                   KFLD                    i20nrs
074300020131     C                   KFLD                    i20nsp
074400020131     C*
074500020131     C     CHIBOL        KLIST
074600020131     C                   KFLD                    VAAS
074700020131     C                   KFLD                    LINPAR
074800020131     C                   KFLD                    VNRS
074900020131     C                   KFLD                    NUMSPE
075000020131     C                   KFLD                    TIPBOL
075100990728     C     CHIBO4        KLIST
075200990728     C                   KFLD                    VAAS
075300990728     C                   KFLD                    LINPAR
075400990728     C                   KFLD                    VNRS
075500990728     C                   KFLD                    NUMSPE
075600990728     C                   KFLD                    KTRC
075700941028     C*
075800941028     C     CHICON        KLIST
075900941028     C                   KFLD                    VAAS
076000941028     C                   KFLD                    LINPAR
076100941028     C                   KFLD                    VNRS
076200941028     C                   KFLD                    NUMSPE
076300941028     C*
076400941028     C     KTBL2         KLIST
076500941028     C                   KFLD                    KKUT
076600941028     C                   KFLD                    KCOD
076700941028     C                   Z-ADD     1             KKUT
076800941028     C*
076900941028     C     CHIACO        KLIST
077000941028     C                   KFLD                    CODUT
077100941028     C                   KFLD                    KCI
077200941028     C                   KFLD                    CODCLI
077300941028     C*
077400941028     C     *LIKE         DEFINE    CSBCDI        CDI
077500941028     C     *LIKE         DEFINE    TBLKUT        KKUT
077600941028     C     *LIKE         DEFINE    TBLCOD        KCOD
077700990728     C     *LIKE         DEFINE    TA4TRC        KTRC
077800121011     C     *LIKE         DEFINE    TBLKEY        KKEY
077900941028     C*
078000941028     C                   Z-ADD     0             CODCLI
078100941028     C                   Z-ADD     0             LINPAR
078200941028     C                   Z-ADD     0             NUMSPE
078300941028     C                   Z-ADD     0             STA               1 0
078400941028     C                   MOVEA     *ZEROS        *IN
078500941028     C                   MOVEL     *BLANKS       TIPBOL
078600941028     C                   Z-ADD     0             VNRS
078700941028     C                   MOVE      WHHDAT        VAAS
078800941028     C*
078900941028     C* Carico tabella cambi/divise
079000941028     C                   Z-ADD     0             XX                3 0
079100941028     C                   MOVEL     'CV'          KCOD
079200941028     C     KTBL2         CHAIN     TABEL                              31
079300941028     C     *IN31         DOWEQ     '0'
079400941028     C     XX            ANDLT     100
079500941028     C     TBLFLG        IFEQ      ' '
079600941028     C                   ADD       1             XX
079700941028     C                   MOVEL     TBLKEY        CCV(XX)
079800941028     C                   MOVEL     TBLUNI        DSCV
079900941028     C                   MOVEL     §CVFDC        FDC(XX)
080000991207     C                   MOVE      §CVDEC        DEC(XX)
080100941028     C                   END
080200941028     C     KTBL2         READE     TABEL                                  31
080300941028     C                   END
080400941028     C*
080500121008     C* Carico tabella tipi incasso
080600121008     C                   Z-ADD     0             XX
080700121008     C                   MOVEL     '1A'          KCOD
080800121008     C     KTBL2         CHAIN     TABEL                              31
080900121008     C     *IN31         DOWEQ     '0'
081000121008     C     XX            ANDLT     200
081100121008     C     TBLFLG        IFEQ      ' '
081200121008     C                   ADD       1             xx
081300121008     C                   MOVEL     TBLUNI        DS1A
081400121008     C                   MOVEL     TBLKEY        C1A(xx)
081500121008     C                   MOVEL     TBLUNI        D1A(xx)
081600121008     C                   END
081700121008     C     KTBL2         READE     TABEL                                  31
081800121008     C                   END
081900121008     C*
082000121008     C*
082100941028     C                   SETOFF                                       284041
082200941028     C                   SETOFF                                       424344
082300121008     c* contrassegno omesso causale 3 abilita indicatore per tipo incasso
082400121008     c     I20CAR        comp      '3'                                    50
082500941031     C                   CLEAR                   TNCSB000
082600991124     C*  RECUPERO DIVISA MONETA CORRENTE
082700991124     C                   CLEAR                   DGED
082800991124     C                   CLEAR                   DSBS02
082900991124     C                   MOVEL     'C'           T02MOD
083000991124     C                   MOVEL     KNSIF         T02SIF
083100991124     C                   MOVEL     'GED'         T02COD
083200991124     C                   MOVEL     '1'           T02KE1
083300991124     C                   CALL      'TIBS02R'
083400991124     C                   PARM                    KPJBA
083500991124     C                   PARM                    DSBS02
083600991124     C*
083700991124    3C     T02ERR        IFEQ      ' '
083800991124     C                   MOVEL     T02UNI        DGED
083900991124    3C                   ENDIF
084000941028     C*
084100941028     C                   ENDSR
084200941028     C*---------------------------------------------------------------*
084300990728     OTITAS000  E            AGG00
084400941028     O                       TASFCA
084500990728     OTITAS010  E            AGG10
084600941028     O                       TASFCA
084700990728     OTITASP00  E            AGGP0
084800941028     O                       TASFCA
084900971218**   SK - SCHIERE PER MESSAGGI
085000941028Contrassegno già esistente in archivio                                        01
085100941028Impossibile immissione operare da gestione bolle                              02
085200941028Linea di partenza inesistente                                                 03
085300941028Numero di spedizione non valido                                               04
085400941028Tipo bolla inesistente                                                        05
085500941028Incongruente tipo bolla e codice cliente                                      06
085600941028Codice cliente non valido                                                     07
085700941028Codice cliente inesistente in anagrafico                                      08
085800140320Anno immesso inferiore di oltre 1 anno da oggi la serie deve essere 99        09
085900941028Linea di arrivo inesistente                                                   10
086000941028Data formalmente errata                                                       11
086100941028Immissione campi mittente obbligatoria                                        12
086200941028Riferimento numerico mittente non valido                                      13
086300941028Riferimento alfabetico mittente non valido                                    14
086400941028Immissione campi destinatario obbligatoria                                    15
086500941028Importo C/Assegno non valido                                                  16
086600941028Codice divisa inesistente                                                     17
086700991207Numero decimali immessi non congruenti con divisa                             18
086800971218Filiale cod. cl. non è congruente con ramo aziendale della  LNP               19
086900140411Anno immesso maggiore di anno corrente                                        20
