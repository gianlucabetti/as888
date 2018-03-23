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
001600971218     D SK              S             78    DIM(19) CTDATA PERRCD(1)
001700941028     D CCV             S              3    DIM(100)
001800941028     D FDC             S              1    DIM(100)
001900991207     D DEC             S              2  0 DIM(100)
001901121008     D D1A             S             78    DIM(200)
001902121008     D C1A             S              2    DIM(200)
002000020201     D com03           S              3    inz
002100941028     D*----------------------------------------------------------------
002200020130     D*  DS PER RICEZIONE PARAMETRI TIBS20R
002300020130     D TIBS20        E DS                  EXTNAME(TIBS20DS)
002400020130     D*  DS PER RICHIAMO TITAS
002500020130     D TNSB50        E DS                  EXTNAME(TNSB50DS)
002600941028     D CSBDS         E DS                  EXTNAME(TNCSB00F)
002700941028     D               E DS                  EXTNAME(TNCSV00F)
002800941028     D  CSVDS                 16    229
002900941028     D DSCV          E DS
003000000000     D PARAM           DS
003100891113     D  LINPAR                 1      3  0
003200891113     D  NUMSPE                 4     10  0
003300941028     D  VAAS                  11     14  0
003400941028     D  VNRS                  15     16  0
003500000000     D TCUDS           DS
003600000000     D  F3                     3      3
003700000000     D  F4                     4      4
003800000000     D  F56                    5      6
003900000000     D KPJBA         E DS
004000000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
004100000000     D  TCU                  398    697
004200000000     D                                     DIM(50)
004300000000     D  KCU                  698    847P 0
004400000000     D                                     DIM(50)
004500000000     D                                     PACKEVEN
004600991124     D DSBS02        E DS                  EXTNAME(TIBS02DS)
004700991124     D DGED          E DS
004800010222     D*FTRUL15    E DSTRUL15DS
004900011113     D Trul21DS      E DS
005000011113     D                                     INZ
005100971217     D TIBS69        E DS                  EXTNAME(TIBS69DS)
005200971217     D CNACO         E DS                  EXTNAME(CNACO00F)
005300971217     D CNIND         E DS                  EXTNAME(CNIND00F)
005400971217     D CNCLP         E DS                  EXTNAME(CNCLP00F)
005500971217     D FNCLS         E DS                  EXTNAME(FNCLS00F)
005501121008     D DSTM          E DS
005502121008     D DS1A          E DS
005600941028     D WLBDA8          DS
005700941028     D  G02DAT                 1      8  0
005800941028     D  G02INV                 9     16  0
005900941028     D  G02ERR                17     17
006000941028     D  G02TGI                18     22  0
006100020208     Dcom07            s              7    inz
006200000000     I*----------------------------------------------------------------
006300941028     IAZORG01L  AA
006400941028     I                             P    4    5 0ORGFIL
006500000120     I                                  7    7  ORGFAG
006600000120     I                                 14   33  ORGDES
006700941028     I                             P 4224 4225 0ORGFC5
006800941028     I*  Tipo Record TNTAS
006900990728     ITITAS000      01
007000990728     ITITAS010      02
007100990728     ITITASP00      03
007200000000     I/SPACE 3
007300000000     C*---------------------------------------------------------------*
007400020130     c* chiamato da pgm TIBS20R
007500121011     c                   if        i20car = '3' or i20car = '8'
007600020130     c                   exsr      srbolla
007700020130     c                   seton                                        04
007800020130     C                   ELSE
007900020130     c                   setoff                                       04
008000020130     C                   CLEAR                   tibs20
008100020130     c                   end
008200020130     c*
008300000000     C     VIDEO         TAG
008400020301     c* se sono chiamato da TIBS20R e c'è errore 1° video esco.
008500020301     c                   if        *in04 and *in28
008600020301     c                   eval      o20msg = $msg
008700020301     c                   goto      fine
008800020301     c                   end
008900941108     C                   SETOFF                                       15
009000941108     C                   MOVEL     *BLANKS       INDMIT
009100941108     C                   MOVEL     *BLANKS       CAPMIT
009200941108     C                   MOVEL     *BLANKS       LOCMIT
009300941108     C                   MOVEL     *BLANKS       DESMIT
009400941108     C                   MOVEL     *BLANKS       PROMIT
009500941108     C                   MOVEL     *BLANKS       NAZMIT
009600941108     C                   Z-ADD     0             DATSPE
009700941108     C                   MOVEL     *BLANKS       DESDES
009800941108     C                   MOVEL     *BLANKS       PRODES
009900941108     C                   MOVEL     *BLANKS       LOCDES
010000941108     C                   MOVEL     *BLANKS       RIFALF
010100011016     C                   MOVEL     §GEDCN        VALUTA
010200941108     C                   Z-ADD     0             RIFMIT
010300941108     C                   Z-ADD     0             IMPORT
010400020131     C* emette il primo video solo se non sono chiamato dal pgm TIBS20R
010500020131     c                   if        not *in04
010600941028     C                   EXFMT     SC03D01
010700020131     c                   end
010800941028     C                   SETOFF                                       284041
010900941028     C                   SETOFF                                       424344
011000971218     C                   SETOFF                                       48
011100941028     C   KC              GOTO      FINE
011200941028     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
011300941028     C                   MOVEL     VAAS          WAA2              2 0
011400941028     C     WAA2          IFEQ      0
011500941028     C     VAAS          IFGT      60
011600941028     C                   ADD       1900          VAAS
011700941028     C                   ELSE
011800941028     C                   ADD       2000          VAAS
011900941028     C                   END
012000941028     C                   END
012100941107     C   KG              GOTO      VISCAS
012200941107     C   KH              GOTO      VISBOL
012300000000     C*
012400900420     C* LINEA DI PARTENZA DEVE ESISTERE SU AZORG
012500941028     C                   Z-ADD     LINPAR        ORGFIL
012600941028     C     ORGFIL        CHAIN     AZORG01L                           21
012700941028     C     *IN21         IFEQ      '1'
012800941028     C                   SETON                                        2840
012900941028     C                   MOVEL     SK(3)         $MSG
013000941028     C                   GOTO      VIDEO
013100941028     C                   END
013200900420     C                   MOVEL     ORGDES        DESCAR
013300000000     C*
013400000000     C* TIPO BOLLA DEVE ESSERE CODIFICATO SU TABEL
013500000000     C                   MOVEL     'TB'          TBLCOD
013600000000     C                   MOVEL     *BLANK        TBLKEY
013700000000     C                   MOVEL     TIPBOL        TBLKEY
013800000000     C     CHITAB        SETLL     TABEL                                  21
013900941028     C     *IN21         IFEQ      '0'
014000941028     C                   SETON                                        2842
014100941028     C                   MOVEL     SK(5)         $MSG
014200941028     C                   GOTO      VIDEO
014300941028     C                   END
014400000000     C                   MOVEL     TIPBOL        BOLBOL            1
014500891113     C                   MOVE      CODCLI        SERV89            4 0
014600000000     C* CONTROLLO CONGRUENZA TIPO BOLLA - CODICE CLIENTE
014700020301     C     SERV89        IFEQ      9999
014800020301     C     BOLBOL        IFNE      'A'
014900020301     C                   SETON                                        284243
015000020301     C                   MOVEL     SK(6)         $MSG
015100020301     C                   GOTO      VIDEO
015200020301     C                   END
015300020301     C                   ELSE
015400020301     C     SERV89        ifeq      8888
015500020301     C     BOLBOL        andne     'F'
015600020301     C                   SETON                                        284243
015700020301     C                   MOVEL     SK(6)         $MSG
015800020301     C                   GOTO      VIDEO
015900020301     C                   END
016000020301     C                   END
017600000000     C*
017700000000     C* NUMERO DI SPEDIZIONE
017800941028     C     NUMSPE        IFEQ      0
017900941028     C                   SETON                                        2841
018000941028     C                   MOVEL     SK(4)         $MSG
018100941028     C                   GOTO      VIDEO
018200941028     C                   END
018300000000     C*
018400941028     C* SE CONTRASSEGNO ESISTE GIA SU TNCSB ERRORE
018500000000     C*
018600941028     C     CHICON        SETLL     TNCSB000                               21
018700941031     C     *IN21         IFEQ      '1'
018800941031     C                   SETON                                        28
018900941031     C                   MOVEL     SK(1)         $MSG
019000941031     C                   GOTO      VIDEO
019100941031     C                   END
019200000000     C*
019300000000     C* CODICE CLIENTE
019400000000     C*
019500941028     C     CODCLI        IFEQ      0
019600941028     C                   SETON                                        2843
019700941028     C                   MOVEL     SK(7)         $MSG
019800941028     C                   GOTO      VIDEO
019900941028     C                   END
020000941107      * CONTROLLO SE LA BOLLA ESISTE
020100941107      * SE ESISTE PRENDO I DATI DELLA BOLLA
020200941107      *
020300941107     C                   SETOFF                                       010203
020400990728     C     CHIBOL        CHAIN(N)  TITAS30C                           31
020500941107     C     *IN31         IFEQ      '0'
020600990728     C                   MOVE      TASRMN        RIFMIT
020700941107     C                   MOVE      TASLNA        LINARR
020800941107     C                   MOVEL     TASAAS        G02INV
020900941107     C                   MOVE      TASMGS        G02INV
021000941107     C                   MOVE      *ZEROS        G02DAT
021100941107     C                   MOVEL     '3'           G02ERR
021200941107     C                   CALL      'XSRDA8'
021300941107     C                   PARM                    WLBDA8
021400941107     C                   MOVE      G02DAT        DATSPE
021500990728     C                   MOVEL     TASRSD        DESDES
021600990728     C                   MOVEL     TASLOD        LOCDES
021700990728     C                   MOVEL     TASPRD        PRODES
021701021120     c                   if        prodes = *blanks
021702021120     c                   movel     tasnzd        prodes
021703021120     c                   end
021800990728     C* Reperisco riferimento mittente alfabetico
021900990728     C                   MOVEL     'A'           KTRC
022000990728     C                   MOVEL     *BLANKS       TA4NOT
022100990728     C     CHIBO4        CHAIN(N)  TITA430C                           34
022200990728     C                   MOVEL     TA4NOT        RIFALF
022201030115     c                   else
022202030115     C*
022203030115     C* CONTROLLO ANNO SPEDIZIONE CHE SIA CORRETTO solo se non trovo la bolla
022204030115     C                   MOVE      WHHDAT        UAAS              4 0
022205030115     C     VAAS          COMP      UAAS                               75  54
022206030115     C  N75
022207030115     CANN54              SUB       1             UAAS
022208030115     C  N75
022209030115     CANN54VAAS          IFNE      UAAS
022210030115     C                   SETON                                        75
022211030115     C                   END
022212030115     C     *IN75         IFEQ      '1'
022213030115     C                   SETON                                        2844
022214030115     C                   MOVEL     SK(9)         $MSG
022215030115     C                   GOTO      VIDEO
022216030115     C                   END
022300941107     C                   ENDIF
022400990728     C***             LINEA DI ARRIVO
022500971217     C                   Z-ADD     LINARR        ORGFIL
022600971217     C     ORGFIL        CHAIN     AZORG01L                           21
022700971217     C     *IN21         IFEQ      '1'
022800971218     C                   SETON                                        2848
022900971217     C                   MOVEL     SK(10)        $MSG
023000971217     C                   GOTO      VIDEO
023100971217     C                   END
023200971217     C                   MOVEL     ORGDES        DESARR
023300971217     C* CONTROLLO ESISTENZA RAGGRUPPAMENTO C/A  VOCE 125 (EX 220)
023400971217     C                   Z-ADD     LINARR        ORGFIL
023500971217     C     ORGFIL        CHAIN     AZORG01L                           84
023600971217     C  N84              Z-ADD     ORGFC5        CSBRGF
023700971217     C     *IN84         IFEQ      '1'
023800000120     C     ORGFAG        ORNE      'F'
023900000120     C     ORGFAG        ANDNE     'A'
024000971218     C                   SETON                                        2848
024100971217     C                   MOVEL     SK(10)        $MSG
024200971217     C                   GOTO      VIDEO
024300971217     C                   END
024400000000     C*
024500000000     C* SELEZIONE CODICE CLIENTE
024600000000     C*
024700891113     C     SERV89        IFNE      8888
024800941107     C     SERV89        ANDNE     9999
024900971217      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE
025000971217     C                   CLEAR                   TIBS69
025100971218     C                   CLEAR                   CNACO
025200971218     C                   CLEAR                   CNIND
025300971218     C                   CLEAR                   CNCLP
025400971218     C                   CLEAR                   FNCLS
025500971217     C                   MOVEL     ' '           I69TLA
025600971217     C                   MOVEL     KNSIF         I69SIF
025700971217     C                   Z-ADD     CODCLI        I69KAC
025800971217     C                   Z-ADD     CODCLI        I69KIN
025900971217     C                   CALL      'TIBS69R'
026000971217     C                   PARM                    TIBS69
026100971217     C                   PARM                    CNACO
026200971217     C                   PARM                    CNIND
026300971217     C                   PARM                    CNCLP
026400971217     C                   PARM                    FNCLS
026500971217     C     O69ERR        IFNE      *BLANK
026600971217     C                   SETON                                        2843
026700971217     C                   MOVEL     O69MSG        $MSG
026800971217     C                   GOTO      VIDEO
026900971217     C                   ENDIF
027000000000     C                   MOVEL     ACORAG        DESMIO
027100000000     C                   MOVEL     INDVIA        INDMIO
027200941028     C                   MOVEL     *BLANKS       CAPMIO
027300941028     C     INDCAP        IFNE      *ZEROS
027400941028     C                   MOVEL     INDCAP        CAPMIO
027500941028     C                   END
027600941028     C     INDCAE        IFNE      *BLANKS
027700941028     C     INDCAE        ANDNE     *ZEROS
027800941028     C                   MOVEL     INDCAE        CAPMIO
027900941028     C                   END
028000000000     C                   MOVEL     INDCIT        LOCMIO
028100941028     C                   MOVEL     INDPRV        PROMIO
028200941028     C                   MOVEL     INDSTA        NAZMIO
028300910930     C                   MOVE      ACOKSC        CDI
028400000000     C                   SETON                                        15
028500000000     C                   END
028600000000     C* EMISSIONE CARICAMENTO CONTRASSEGNO
028700000000     C*
028800000000     C     DISPLY        TAG
028900000000     C*
029000941028     C                   EXFMT     SC03D02
029100941028     C                   SETOFF                                       284041
029200941028     C                   SETOFF                                       424344
029300941028     C                   SETOFF                                       454647
029400121023     C                   SETOFF                                       4849
029500941028     C   KC              GOTO      FINE
029600020131     c* se chiamato da TIBS20R e premo f12 torno al pgm chiamante
029700020131     c                   if        *inkl
029800020201     c                   if        *in04
029900020131     c                   goto      fine
030000020131     c                   else
030100020131     C                   GOTO      VIDEO
030200020131     c                   end
030300020131     c                   end
030400000000     C*
030500900420     C* CONTROLLI:
030600000000     C*       DATA SPEDIZIONE
030700941028     C                   CLEAR                   WLBDA8
030800941028     C                   Z-ADD     DATSPE        G02DAT
030900941028     C                   CALL      'XSRDA8'
031000941028     C                   PARM                    WLBDA8
031100941028     C     G02ERR        IFEQ      '1'
031200941028     C                   SETON                                        2841
031300941028     C                   MOVEL     SK(11)        $MSG
031400941028     C                   GOTO      DISPLY
031500941028     C                   END
031600941028     C                   Z-ADD     G02DAT        DATSPE
031700891113     C* SE CLIENTE 8888/9999 VOCI MITTENTE OBBLIGATORIE
031800000000     C     *IN15         IFEQ      '0'
031900941028     C     DESMIT        IFEQ      *BLANKS
032000941028     C     INDMIT        OREQ      *BLANKS
032100941028     C     LOCMIT        OREQ      *BLANKS
032200941028     C     PROMIT        OREQ      *BLANKS
032300941028     C                   SETON                                        2842
032400941028     C                   MOVEL     SK(12)        $MSG
032500941028     C                   GOTO      DISPLY
032600000000     C                   END
032700941031     C                   END
032800000000     C* CAMPI DESTINATARIO OBBLIGATORI
032900941031     C     DESDES        IFEQ      *BLANKS
033000941031     C     PRODES        OREQ      *BLANKS
033100941031     C     LOCDES        OREQ      *BLANKS
033200941028     C                   SETON                                        2845
033300941028     C                   MOVEL     SK(15)        $MSG
033400941028     C                   GOTO      DISPLY
033500941028     C                   END
033600000000     C* RIFERIMENTI MITTENTE OBBLIGATORIO
033700941028     C     RIFMIT        IFEQ      0
033800941028     C                   SETON                                        2843
033900941028     C                   MOVEL     SK(13)        $MSG
034000941028     C                   GOTO      DISPLY
034100941028     C                   END
034200000000     C* IMPORTO CONTRASSEGNO
034300941028     C     IMPORT        IFEQ      0
034400941028     C                   SETON                                        2846
034500941028     C                   MOVEL     SK(16)        $MSG
034600941028     C                   GOTO      DISPLY
034700941028     C                   END
034800941028     C*
034900941028     C                   Z-ADD     1             XX                3 0
035000941028     C     VALUTA        LOOKUP    CCV(XX)                                31
035100941028     C     *IN31         IFEQ      '0'
035200941028     C                   SETON                                        4728
035300941028     C                   MOVEL     SK(17)        $MSG
035400941028     C                   ELSE
035500941028     C                   MOVE      IMPORT        COMO3             3 0
035600991207     C                   MOVE      IMPORT        COMO2             2 0
035700991207     C                   MOVE      IMPORT        COMO1             1 0
035800941028     C     COMO3         IFNE      *ZEROS
035900941028     C     FDC(XX)       ANDNE     'S'
036000941028     C     COMO3         ORNE      *ZEROS
036100991124     C     VALUTA        ANDEQ     *BLANKS
036200991207     C     COMO1         ORNE      *ZEROS
036300991207     C     DEC(XX)       ANDLT     3
036400991207     C     COMO2         ORNE      *ZEROS
036500991207     C     DEC(XX)       ANDLT     2
036600941028     C                   SETON                                        4728
036700941028     C                   MOVEL     SK(18)        $MSG
036800941028     C                   END
036900941028     C                   END
036901121008     C* tipo incasso contrassegno solo se causale = '3' C/A omesso
036902121011     c                   clear                   o20tinc
036903121008     C                   IF        tipoinc <> *blank
036904121008     C                   exsr      vertipoinc
036905121008     C                   if        *in49
036906121011     c                   clear                   o20tinc
036907121008     c                   seton                                        28
036908121008     C                   eval      $msg = 'Tipo incasso non valido'
036909121008     C                   GOTO      DISPLY
036910121011     c                   else
036911121011     c                   eval      o20tinc = tipoinc
036912121008     C                   Endif
036913121008     C                   END
036914121008     C*
037000011113     ** Controllo importo massimo contrassegno.
037100011113     C                   EXSR      ChkCasMax
037200011113     C                   IF        O21Fx2 <> *BLANK
037300011114     C                             OR
037400011114     C                             O21FMn <> *BLANK
037500011113     C                   SETON                                        2846
037600011113     C                   EVAL      $Msg =
037700011114     C                             'Immettere un importo compreso fra '
037800011114     C                             + %TRIM(%EDITC(O21LMn:'2')) + ' ' + §GEDMc
037900011114     C                             + ' e '
038000011113     C                             + %TRIM(%EDITC(O21Lx2:'2')) + ' ' + §GEDMc
038100011113     C                             + '.'
038200011113     C                   GOTO      Disply
038300011114     C                   ENDIF
038400000000     C* FINE CONTROLLI----------------------------------------------------
038500941028     C  NKF              GOTO      DISPLY
038600000000     C* REGISTRAZIONE CONTRASSEGNO
038700020131     C                   MOVE      046           CSBFLE
038800910912     C                   MOVEL     TIPBOL        CSBTBL
038900910912     C                   Z-ADD     LINPAR        CSBLNP
039000910912     C                   Z-ADD     NUMSPE        CSBNSP
039100941028     C                   Z-ADD     G02INV        CSBMGS
039200910912     C                   Z-ADD     CODCLI        CSBKSC
039300941028     C                   Z-ADD     IMPORT        CSBCAS
039400941108     C                   MOVEL     VALUTA        CSBVCA
039500941028     C                   MOVEL     *BLANKS       CSBNRA
039600910912     C                   Z-ADD     0             CSBDTE
039700910912     C                   Z-ADD     0             CSBBNA
039800910912     C                   Z-ADD     LINARR        CSBLNA
039900910912     C                   Z-ADD     0             CSBDTA
040000910912     C                   Z-ADD     0             CSBDDC
040100910912     C                   Z-ADD     0             CSBNDT
040200910912     C                   Z-ADD     STA           CSBSTA
040300910912     C                   MOVEL     *ZEROS        CSBABI
040400910912     C                   MOVEL     *ZEROS        CSBCAB
040500910912     C                   MOVEL     *BLANK        CSBTPA
040600910912     C                   MOVEL     *BLANK        CSBTPI
040700910912     C                   MOVEL     *BLANK        CSBFIN
040800910912     C                   MOVEL     *BLANK        CSBFPC
040900910912     C                   MOVEL     *ZEROS        CSBNDP
041000910912     C                   MOVEL     *ZEROS        CSBDDP
041100910912     C                   MOVEL     *ZEROS        CSBNRG
041200910912     C                   MOVEL     *ZEROS        CSBDRG
041300931108     C                   MOVEL     'R'           CSBFTR
041400931108     C                   MOVE      DATEU         CSBDTR
041500910912     C                   MOVEL     *ZEROS        CSBFGC
041600910912     C                   MOVEL     *ZEROS        CSBNGC
041700910912     C                   MOVEL     *ZEROS        CSBDGC
041800910912     C                   MOVEL     *BLANK        CSBMAS
041900910912     C                   MOVEL     *BLANK        CSBFUS
042000910913     C   15              Z-ADD     CDI           CSBCDI
042100910913     C  N15              Z-ADD     CSBKSC        CSBCDI
042200910912     C                   Z-ADD     RIFMIT        CSBRMN
042300941028     C                   MOVEL     RIFALF        CSBRMA
042400910912     C                   MOVEL     DESDES        CSBRSD
042500910912     C                   MOVEL     LOCDES        CSBPAR
042600910912     C                   MOVEL     PRODES        CSBPRO
042700941028     C                   MOVEL     G02INV        CSBAAS
042800910912     C                   MOVE      *ZEROS        CSBUDA
042900910912     C                   Z-ADD     VNRS          CSBNRS
043000000000     C*
043100941028     C                   WRITE     TNCSB000
043200941116     C*
043300941116     C*  Aggiorno Flag su TNTAS
043400941116     C                   SETOFF                                       010203
043500990728     C     CHIBOL        CHAIN     TITAS30C                           31
043600941116     C     *IN31         IFEQ      '0'
043700941116     C                   MOVEL     'S'           TASFCA
043800941116     C   01              EXCEPT    AGG00
043900941116     C   02              EXCEPT    AGG10
044000941116     C   03              EXCEPT    AGGP0
044100941116     C                   END
044200910912     C* SCRIVO STORICO
044300910912     C                   MOVE      046           CSVFEV
044400020201     c                   if        *in04 and %subst(knmus: 1: 3) = 'FIL'
044500020201     c                   eval      com03 =  %subst(knmus: 4: 3)
044600020201     c                   move      com03         csvfle
044700020131     c                   end
044800910912     C                   MOVEL     'IMCA'        CSVCAV
044900910912     C                   TIME                    CSVORV
045000910912     C                   MOVE      DATEU         CSVDTV
045100910912     C                   MOVEL     CSBDS         CSVDS
045200941028     C                   WRITE     TNCSV000
045300910912     C*
045400000000     C     *IN15         IFEQ      '0'
045500891113     C* CREO BLCAN00F SE CODICE CLIENTE 8888/9999
045600910912     C                   Z-ADD     LINPAR        CSALNP
045700910912     C                   Z-ADD     NUMSPE        CSANSP
045800910912     C                   MOVEL     DESMIT        CSARSD
045900910912     C                   MOVEL     INDMIT        CSAVID
046000941028     C                   MOVEL     CAPMIT        CSACAD
046100910912     C                   MOVEL     LOCMIT        CSACID
046200941028     C                   MOVEL     PROMIT        CSAPRM
046300941028     C                   MOVEL     NAZMIT        CSANZM
046400910912     C                   Z-ADD     VNRS          CSANRS
046500941028     C                   MOVEL     G02INV        CSAAAS
046600000000     C*
046700941028     C                   WRITE     TNCSA000
046800000000     C                   END
046900000000     C                   GOTO      FINE
047000000000     C     VISBOL        TAG
047100000000     C*
047200941104     C                   MOVEL     KPJBU         KPJBUS          256
047300941107     C                   MOVEL     *BLANK        KPJBU
047400990728     C                   CLEAR                   TNSB50
047500990728     C                   MOVEL     'C01'         I50OP0
047600990728     C                   MOVEL     TNSB50        KPJBU
047700990728     C                   CALL      'TNSB50R'
047800941104     C                   PARM                    KPJBA
047900990728     C                   MOVEL     KPJBU         TNSB50
048000990728     C                   MOVE      D50AAS        VAAS
048100990728     C                   MOVE      D50LNP        LINPAR
048200990728     C                   MOVE      D50NRS        VNRS
048300990728     C                   MOVE      D50NSP        NUMSPE
048400990728     C                   MOVE      D50TBL        TIPBOL
048500941104     C                   MOVEL     KPJBUS        KPJBU
048600000000     C                   GOTO      VIDEO
048700941107     C     VISCAS        TAG
048800941107     C*
048900941107     C                   MOVEL     KPJBU         KPJBUS          256
049000941107     C                   MOVEL     *BLANK        KPJBU
049100951211     C                   CALL      'TNSC40R'
049200941107     C                   PARM                    KPJBA
049300941107     C                   MOVEL     KPJBUS        KPJBU
049400941107     C                   GOTO      VIDEO
049500000000     C     FINE          TAG
049600020131     c*
049700941031     C  NKF              MOVEL     *ZEROS        PARAM
049800000000     C                   MOVEL     PARAM         KPJBU
049900020131     c*
050000020131     c                   if        *in04
050100020131     c                   if        not *inkf
050200020131     c                   movel     '1'           o20err
050300020131     c                   end
050400020131     c                   movel(p)  tibs20        kpjbu
050500020131     c                   end
050600020131     c*
050700000000     C                   Z-ADD     0             LINPAR
050800000000     C                   Z-ADD     0             NUMSPE
050900891113     C                   Z-ADD     0             VAAS
051000891113     C                   Z-ADD     0             VNRS
051100941028     C*
051200971218     C                   CLEAR                   TIBS69
051300971218     C                   MOVEL     'C'           I69TLA
051400971218     C                   CALL      'TIBS69R'
051500971218     C                   PARM                    TIBS69
051600971218     C                   PARM                    CNACO
051700971218     C                   PARM                    CNIND
051800971218     C                   PARM                    CNCLP
051900971218     C                   PARM                    FNCLS
052000910913     C                   SETON                                        LR
052100011113
052200011113     ** Controllo importo massimo contrassegno.
052300011113     C     ChkCasMax     BEGSR
052400011113     C*
052500011113     C                   CLEAR                   Trul21DS
052600011114     C*
052700011114     C                   IF        %FOUND(TiTas30C)
052800011114     C                   EVAL      I21TSp = TasTSp
052900011113     C                   EVAL      I21NzD = TasNzD
053000011113     C                   MOVE      TasCTr        I21CTr
053100011114     C                   ENDIF
053200011114     C*
053300011114     C                   EVAL      I21CBo = TipBol
053400011114     C                   EVAL      I21LnP = LinPar
053500011114     C                   EVAL      I21LnA = LinArr
053600011114     C                   EVAL      I21Ksc = CodCli
053700011114     C                   EVAL      I21NzM = NazMit
053800011114     C                   EVAL      I21TiC = *BLANK
053900011113     C                   EVAL      I21Imp = Import
054000011113     C                   EVAL      I21Div = Valuta
054100011113     C                   EVAL      I21Ute = KNmUs
054200011113     C                   EVAL      I21Pgm = NomPgm
054300011114     C*
054400011113     C                   CALL      'TRUL21R'
054500011113     C                   PARM                    Trul21DS
054600011113     C*
054700011113     C                   ENDSR
054701121008     C*---------------------------------------------------------------*
054704121008     C     vertipoinc    BEGSR
054705121008     C*---------------------------------------------------------------*
054706121008     C*  Controllo se richiesta ricerca tipo incasso
054707121023     C     '?'           SCAN      tipoinc                                16
054708121023     C     *IN16         IFEQ      '1'
054709121011     C                   MOVEL     KKUT          kKUT
054710121011     C                   MOVEL     '1A'          kCOD
054711121008     C                   MOVE      *BLANKS       tipoinc
054712121008     C                   CALL      'X§TABER'
054713121011     C                   PARM                    kKUT
054714121011     C                   PARM                    kCOD
054715121011     C                   PARM                    kkey
054716121011     C                   PARM                    §DES             30
054717121011     C                   MOVEL     kkey          tipoinc
054718121008     C                   END
054719121008     C                   Z-ADD     1             X
054720121008     C     tipoinc       LOOKUP    C1A(X)                                 95
054721121008     C     *IN95         IFEQ      '0'
054722121008     C                   SETON                                        49
054724121008     C                   GOTO      UPDRE1
054725121008     C                   endif
054726121008     C                   MOVEL     D1A(X)        DS1A
054727121008     C     tipoInc       IFEQ      'TM'
054728121008     C     tipoInc       OREQ      'TO'
054729121008     C     tipoInc       OREQ      'BB'
054730121008     C     tipoInc       OREQ      'BC'
054731121008     C     tipoInc       OREQ      'CB'
054732121008     C     tipoInc       OREQ      'BA'
054733121008     C     tipoInc       OREQ      'CA'
054734121008     C     tipoInc       OREQ      'BP'
054735121008     C                   CLEAR                   DSTM
054736130328     C                   MOVEL     'TM'          TBLCOD
054737130328     C                   MOVEL     *BLANK        TBLKEY
054738130328     C                   MOVEL     codcli        TBLKEY
054739130411     C     CHITAB        chain     TABEL                              21
054740130328     C     *IN21         IFEQ      '0'
054747130328     C                   movel     tbluni        DSTM
054748130328     c                   endif
054749121008     C*
054750121008     C     tipoinc       IFEQ      'TM'
054751121008     C     §TMFTM        ANDNE     'S'
054752121008     C     *IN95         ANDEQ     '1'
054753121008     C     tipoinc       OREQ      'BB'
054754121008     C     §TMFBB        ANDNE     'S'
054755121008     C     *IN95         ANDEQ     '1'
054756121008     C     tipoinc       OREQ      'TO'
054757121008     C     §TMFTO        ANDNE     'S'
054758121008     C     *IN95         ANDEQ     '1'
054759121008     C     tipoinc       OREQ      'BB'
054760121008     C     §TMFBC        ANDEQ     'S'
054761121008     C     *IN95         ANDEQ     '1'
054762121008     C     tipoinc       OREQ      'BC'
054763121008     C     §TMFBC        ANDNE     'S'
054764121008     C     *IN95         ANDEQ     '1'
054765121008     C     tipoinc       OREQ      'CB'
054766121008     C     §TMFBB        ANDNE     'S'
054767121008     C     *IN95         ANDEQ     '1'
054768121008     C     tipoinc       OREQ      'CB'
054769121008     C     §TMFBC        ANDEQ     'S'
054770121008     C     *IN95         ANDEQ     '1'
054771121008     C     tipoinc       OREQ      'BA'
054772121008     C     §TMFBA        ANDNE     'S'
054773121008     C     *IN95         ANDEQ     '1'
054774121008     C     tipoinc       OREQ      'CA'
054775121008     C     §TMFCA        ANDNE     'S'
054776121008     C     *IN95         ANDEQ     '1'
054777121008     C     tipoinc       OREQ      'BP'
054778121008     C     §TMFbp        ANDNE     'S'
054779121008     C     *IN95         ANDEQ     '1'
054780121008     C     *IN95         OREQ      '0'
054781121008     C                   SETON                                        49
054782121008     C                   GOTO      UPDRE1
054783121008     C                   END
054784121008     C                   END
054785121008     C*
054786121008     C     updre1        ENDSR
054800000000     C*---------------------------------------------------------------*
054900020130     C* AGGANCIO BOLLA                                                *
055000941028     C*---------------------------------------------------------------*
055100020130     C     SRBOLLA       BEGSR
055200020130     C*
055300020131     c     kbol20        chain     titas30c
055400020131     c                   if        %found(titas30c)
055500020130     c                   eval      linpar = i20lnp
055600020130     c                   eval      vnrs = i20nrs
055700020130     c                   eval      numspe = i20nsp
055800020131     c                   eval      tipbol = tastbl
055900020204     c                   eval      vaas = i20aas
056000020204     c                   eval      linarr = i20lna
056100020204     c                   eval      codcli = tasccm
056200020301     c                   move      tasccm        serv89
056300020301     c                   if        %subst(tastbl: 1: 1) = 'A' and
056400020301     c                             serv89 = 8888
056500020301     c                   move      linarr        com03             3
056600020301     c                   eval      com07 = com03 + '9999'
056700020301     c                   move      com07         codcli
056800020301     c                   end
056900020131     c* stato contrassegno
057000020131     C                   Z-ADD     3             STA
057100020131     c                   else
057200020201     c                   movel     '1'           o20err
057300020201     c                   movel(p)  tibs20        kpjbu
057400020131     c                   exsr      uscita
057500020131     c                   end
057600020130     C*
057700020130     C                   ENDSR
057800020131     ***********************************************************************
057900020131     ** Uscita dal programma.
058000020131     ***********************************************************************
058100020131     C     Uscita        BEGSR
058200020131     C*
058300020131     C                   EVAL      *INLR = *ON
058400020131     C                   RETURN
058500020131     C*
058600020131     C                   ENDSR
058700020130     C*---------------------------------------------------------------*
058800020130     C* OPERAZIONI INIZIALI                                           *
058900020130     C*---------------------------------------------------------------*
059000020130     C     *INZSR        BEGSR
059100020130     C*
059200941028     C     *ENTRY        PLIST
059300941028     C                   PARM                    KPJBA
059400020208     c                   if        kpjbu <> *blanks
059500020208     c                   movel     kpjbu         TIBS20
059600020208     c                   else
059700020208     c                   clear                   tibs20
059800020208     c                   end
059900941028     C                   Z-ADD     1             CODUT
060000941028     C                   CALL      'XPARUT'
060100941028     C                   PARM                    UTEDSE
060200941028     C     CODUT         CABEQ     -1            FINE
060300941028     C                   MOVEL     RAGUT         RSUT             20
060400941028     C                   MOVEL     RAGUT         RSUT2            25
060500941028     C*--- RICERCA CAPOCONTI------------------------------------------*
060600941028     C                   DO        50            X                 2 0
060700941028     C                   MOVE      TCU(X)        TCUDS
060800941031     C     F56           IFEQ      'CG'
060900941028     C     F4            ANDEQ     '1'
061000941031     C     F3            ANDEQ     '0'
061100941028     C                   Z-ADD     KCU(X)        KCI               4 0
061200941028     C                   END
061300941028     C                   END
061400941028     C*---------------------------------------------------------------*
061500941028     C* GIRO UDATE
061600941028     C                   CLEAR                   WLBDA8
061700941028     C                   TIME                    WHHDAT           14 0
061800941028     C                   MOVE      WHHDAT        G02DAT
061900941028     C                   CALL      'XSRDA8'
062000941028     C                   PARM                    WLBDA8
062100941028     C                   Z-ADD     G02INV        DATEU             8 0
062200941028     C*
062300941028     C* DEFINIZIONE CHIAVI
062400941028     C*
062500941028     C     CHITAB        KLIST
062600941028     C                   KFLD                    CODUT
062700941028     C                   KFLD                    TBLCOD
062800941028     C                   KFLD                    TBLKEY
062900941028     C*
063000020131     C     kbol20        KLIST
063100020131     C                   KFLD                    i20aas
063200020131     C                   KFLD                    i20lnp
063300020131     C                   KFLD                    i20nrs
063400020131     C                   KFLD                    i20nsp
063500020131     C*
063600020131     C     CHIBOL        KLIST
063700020131     C                   KFLD                    VAAS
063800020131     C                   KFLD                    LINPAR
063900020131     C                   KFLD                    VNRS
064000020131     C                   KFLD                    NUMSPE
064100020131     C                   KFLD                    TIPBOL
064200990728     C     CHIBO4        KLIST
064300990728     C                   KFLD                    VAAS
064400990728     C                   KFLD                    LINPAR
064500990728     C                   KFLD                    VNRS
064600990728     C                   KFLD                    NUMSPE
064700990728     C                   KFLD                    KTRC
064800941028     C*
064900941028     C     CHICON        KLIST
065000941028     C                   KFLD                    VAAS
065100941028     C                   KFLD                    LINPAR
065200941028     C                   KFLD                    VNRS
065300941028     C                   KFLD                    NUMSPE
065400941028     C*
065500941028     C     KTBL2         KLIST
065600941028     C                   KFLD                    KKUT
065700941028     C                   KFLD                    KCOD
065800941028     C                   Z-ADD     1             KKUT
065900941028     C*
066000941028     C     CHIACO        KLIST
066100941028     C                   KFLD                    CODUT
066200941028     C                   KFLD                    KCI
066300941028     C                   KFLD                    CODCLI
066400941028     C*
066500941028     C     *LIKE         DEFINE    CSBCDI        CDI
066600941028     C     *LIKE         DEFINE    TBLKUT        KKUT
066700941028     C     *LIKE         DEFINE    TBLCOD        KCOD
066800990728     C     *LIKE         DEFINE    TA4TRC        KTRC
066801121011     C     *LIKE         DEFINE    TBLKEY        KKEY
066900941028     C*
067000941028     C                   Z-ADD     0             CODCLI
067100941028     C                   Z-ADD     0             LINPAR
067200941028     C                   Z-ADD     0             NUMSPE
067300941028     C                   Z-ADD     0             STA               1 0
067400941028     C                   MOVEA     *ZEROS        *IN
067500941028     C                   MOVEL     *BLANKS       TIPBOL
067600941028     C                   Z-ADD     0             VNRS
067700941028     C                   MOVE      WHHDAT        VAAS
067800941028     C*
067900941028     C* Carico tabella cambi/divise
068000941028     C                   Z-ADD     0             XX                3 0
068100941028     C                   MOVEL     'CV'          KCOD
068200941028     C     KTBL2         CHAIN     TABEL                              31
068300941028     C     *IN31         DOWEQ     '0'
068400941028     C     XX            ANDLT     100
068500941028     C     TBLFLG        IFEQ      ' '
068600941028     C                   ADD       1             XX
068700941028     C                   MOVEL     TBLKEY        CCV(XX)
068800941028     C                   MOVEL     TBLUNI        DSCV
068900941028     C                   MOVEL     §CVFDC        FDC(XX)
069000991207     C                   MOVE      §CVDEC        DEC(XX)
069100941028     C                   END
069200941028     C     KTBL2         READE     TABEL                                  31
069300941028     C                   END
069400941028     C*
069401121008     C* Carico tabella tipi incasso
069402121008     C                   Z-ADD     0             XX
069403121008     C                   MOVEL     '1A'          KCOD
069404121008     C     KTBL2         CHAIN     TABEL                              31
069405121008     C     *IN31         DOWEQ     '0'
069406121008     C     XX            ANDLT     200
069407121008     C     TBLFLG        IFEQ      ' '
069408121008     C                   ADD       1             xx
069409121008     C                   MOVEL     TBLUNI        DS1A
069411121008     C                   MOVEL     TBLKEY        C1A(xx)
069412121008     C                   MOVEL     TBLUNI        D1A(xx)
069417121008     C                   END
069418121008     C     KTBL2         READE     TABEL                                  31
069419121008     C                   END
069420121008     C*
069435121008     C*
069500941028     C                   SETOFF                                       284041
069600941028     C                   SETOFF                                       424344
069601121008     c* contrassegno omesso causale 3 abilita indicatore per tipo incasso
069602121008     c     I20CAR        comp      '3'                                    50
069700941031     C                   CLEAR                   TNCSB000
069800991124     C*  RECUPERO DIVISA MONETA CORRENTE
069900991124     C                   CLEAR                   DGED
070000991124     C                   CLEAR                   DSBS02
070100991124     C                   MOVEL     'C'           T02MOD
070200991124     C                   MOVEL     KNSIF         T02SIF
070300991124     C                   MOVEL     'GED'         T02COD
070400991124     C                   MOVEL     '1'           T02KE1
070500991124     C                   CALL      'TIBS02R'
070600991124     C                   PARM                    KPJBA
070700991124     C                   PARM                    DSBS02
070800991124     C*
070900991124    3C     T02ERR        IFEQ      ' '
071000991124     C                   MOVEL     T02UNI        DGED
071100991124    3C                   ENDIF
071200941028     C*
071300941028     C                   ENDSR
071400941028     C*---------------------------------------------------------------*
071500990728     OTITAS000  E            AGG00
071600941028     O                       TASFCA
071700990728     OTITAS010  E            AGG10
071800941028     O                       TASFCA
071900990728     OTITASP00  E            AGGP0
072000941028     O                       TASFCA
072100971218**   SK - SCHIERE PER MESSAGGI
072200941028Contrassegno già esistente in archivio                                        01
072300941028Impossibile immissione operare da gestione bolle                              02
072400941028Linea di partenza inesistente                                                 03
072500941028Numero di spedizione non valido                                               04
072600941028Tipo bolla inesistente                                                        05
072700941028Incongruente tipo bolla e codice cliente                                      06
072800941028Codice cliente non valido                                                     07
072900941028Codice cliente inesistente in anagrafico                                      08
073000941028Anno di spedizione errato                                                     09
073100941028Linea di arrivo inesistente                                                   10
073200941028Data formalmente errata                                                       11
073300941028Immissione campi mittente obbligatoria                                        12
073400941028Riferimento numerico mittente non valido                                      13
073500941028Riferimento alfabetico mittente non valido                                    14
073600941028Immissione campi destinatario obbligatoria                                    15
073700941028Importo C/Assegno non valido                                                  16
073800941028Codice divisa inesistente                                                     17
073900991207Numero decimali immessi non congruenti con divisa                             18
074000971218Filiale cod. cl. non è congruente con ramo aziendale della  LNP               19
