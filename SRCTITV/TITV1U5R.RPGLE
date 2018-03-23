000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200100125     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020916     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700000724     D*------------
000800990920     D dscmz         e ds                  inz
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200941229     D* 10+3
001300941229     D                 DS
001400000313     D  WI15                   1     10
001500000313     D  WD15                  11     13
001600000313     D  WASC15                 1     13
001700941229     D*  6+1
001800941229     D                 DS
001900000313     D  WI09                   1      6
002000000313     D  WD09                   7      7
002100000313     D  WASC09                 1      7
002200941229     D*  2+3
002300941229     D                 DS
002400000313     D  WI07                   1      2
002500000313     D  WD07                   3      5
002600000313     D  WASC07                 1      5
002700000313     D*
002800990920     D tivlrds       e ds                  extname(tivlr00f)
002900041025     D tisi95ds      e ds
003000000313     D*---------------------------------------------------------
003100000313     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
003200000313     D*---------------------------------------------------------
003300000313     D                 ds
003400000313     D vhbatb                  1      1
003500000313     D vhbccm                  2      8
003600000313     D vhblnp                  9     11
003700000313     D vhbaas                 12     15
003800000313     D vhbmgs                 16     19
003900000313     D vhbnrs                 20     21
004000000313     D vhbnsp                 22     28
004100000313     D vhbcbo                 29     30
004200000313     D vhblna                 31     33
004300000313     D vhbrsd                 34     68
004400000313     D vhbrd2                 69    103
004500000313     D vhbind                104    138
004600000313     D vhbcad                139    147
004700000313     D vhblod                148    182
004800000313     D vhbprd                183    184
004900000313     D vhbnzd                185    187
005000000313     D vhbgc1                188    189
005100000313     D vhbgc2                190    191
005200000313     D vhbctr                192    194
005300000313     D vhbtsp                195    195
005400000313     D vhbias                196    208
005500000313     D vhbvas                209    211
005600000313     D vhbnas                212    226
005700000313     D vhbncl                227    231
005800000313     D vhbpkb                232    238
005900000313     D vhbvlb                239    243
006000000313     D vhbqft                244    256
006100000313     D vhbcas                257    269
006200000313     D vhbtic                270    271
006300000313     D vhbvca                272    274
006400000313     D vhbgca                275    276
006500000313     D vhbrmn                277    291
006600000313     D vhbrma                292    306
006700000313     D vhbncd                307    313
006800000313     D vhbnca                314    320
006900000313     D vhbxco                321    321
007000000313     D vhbnot                322    356
007100000313     D vhbnt2                357    391
007200000313     D vhbznc                392    393
007300000313     D vhbctm                394    395
007400000313     D vhbffd                396    396
007500000313     D vhbdcr                397    404
007600000313     D vhbtcr                405    405
007700000313     D vhbhcr                406    409
007800000313     D vhbcts                410    411
007900000313     D vhbftm                412    412
008000000313     D vhbvmd                413    425
008100000313     D vhbvad                426    428
008200000313     D vhbgma                429    430
008300000313     D vhbgga                431    432
008400000313     D vhbgva                433    434
008500000313     D vhbtc1                435    435
008600000313     D vhbtc2                436    436
008700000313     D vhbscl                437    437
008800000313     D vhbant                438    446
008900000313     D vhbrmo                447    471
009000000313     D vhbcmo                472    480
009100000313     D vhbnmo                481    483
009200000313     D  X_rec                  1    483    inz
009300000313     D*
009400990910     D esito           s              1
009500000724     D prmlit          s             10
009600000710     D prmfir          s             10
009700990921     D wrkesito        s                   like(esito)
009800000613     D rrnum           s              6  0 INZ(*zeros)
009900000313     D*
010000100125
010100100125
010200100125     D*------------------
010300100125     D* LINKING A DEFINIZIONI ESTERNE
010400100125     D*------------------
010500100125     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010600100125     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010700100125
010800100125
010900010606     D*
011000040407     D TxtInOut        S           2048
011100040407     D ElencoChar      S            256
011200040407     D TipoElenco      S              1
011300040407     D CharSost        S              1
011400040407     D UpperCase       S              1
011500040407     D ChkNull         S              1
011600040407     D CharNull        S              1
011700040407     D EsitoBon        S              1
011800010606
011900010606
012000010606
012100010606
012200000913     C                   reset                   rrnum
012300990921     C                   reset                   esito
012400990921     C                   reset                   wrkesito
012500010601     C*
012600010601     C                   exsr      opeini
012700010601     C                   exsr      rwvab
012800041025     C*
012900041025     C* Esegue lancio TISI95R solo x chiusura
013000041025     C                   CLEAR                   TISI95DS
013100041025     C                   EVAL      I95TLA = 'C'
013200041025     C                   CALL      'TISI95R'
013300041025     C                   PARM                    TISI95DS
013400010601     C*
013500010601     C                   seton                                        lr
013600010601
013700010601
013800010601
013900010601
014000010601     C*--------------------------------------------------------
014100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014200010601     C*--------------------------------------------------------
014300010601     C     PREELA        BEGSR
014400010601     C*
014500010601     C* SE OCCORRE SPEDIRE IN FILIALE
014600010601     C                   if        invfil <> *zeros and
014700010601     C                             flgGiro = '0'
014800010601     C*
014900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
015000010601     C                   eval      flgGiro = '1'
015100010601     C*
015200010601     C                   endif
015300010601     C*
015400010601     C                   ENDSR
015500010601     C***
015600010601
015700010601
015800010601
015900010601
016000010601
016100010601
016200010601     C*--------------------------------------------------------
016300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016400010601     C*--------------------------------------------------------
016500010601     C     ENDELA        BEGSR
016600010601     C*
016700010601     C                   ENDSR
016800010601     C***
016900000613
017000010601
017100010601
017200010601
017300010601
017400910830     C*--------------------------------------------------------
017500020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
017600910830     C*--------------------------------------------------------
017700941229     C     RWVAB         BEGSR
017800990910     C*
017900990914     C                   if        not %open(tivin00r)
018000990908     C                   open      tivin00r
018100990914     C                   endif
018200020916     C                   if        not %open(fivabwwr)
018300020916     C                   open      fivabwwr
018400990914     C                   endif
018500990910     C*
018600990910     C                   clear                   §CTROK
018700990910     C                   clear                   §CTRMO
018800990910     C                   clear                   §CTRNO
018900990910     C*
019000921023     C                   DO        *HIVAL
019100990913     C*
019200990915     C                   READ      tivin00r                               70
019300010618     C*
019400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019500010618     C                   if        vinflg = '1'
019600010618     C                   eval      flgOk = '1'
019700010618     C                   endif
019800010618     C*
019900051017     C                   if        vindta  > *blanks
020000000613     C                   add       1             rrnum
020100990913     C*
020200010601     C                   if        *in70 = *off and
020300990913     C                             (vinflg = *blanks
020400010711     C                              or vinflg = '0'
020500010711     C                              or vinflg = '2')
020600990913     C*
020700020916     C                   clear                   FIVAB000
020800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020900010711     C                   if        vinflg = *blanks or vinflg = '0'
021000010711     C                   clear                   vinmsg
021100010711     C                   endif
021200010711     C*
021300000313     C                   eval      x_rec  = vindta
021400991022     C*
021500991022     C                   testb     x'3F'         vhbatb                   02
021600991022     C                   if        not *in02
021700991022     C*
021800971021     C                   SETOFF                                       303132
021900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
022000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
022100971023     C*    OPPURE TUTTI BLANKS
022200010601     C*
022300010601     C                   TESTN                   VHBLNP               30
022400010601     C  N30              TESTN                   VHBLNP                 30
022500010601     C  N30              TESTN                   VHBLNP                   30
022600010601     C                   MOVE      VHBLNP        W001A             1
022700010601     C     *IN30         IFEQ      *OFF
022800010601     C     W001A         ORLT      '0'
022900010601     C                   SETON                                        31
023000010601     C                   eval      vinmsg = %trimr(vinmsg)
023100010601     C                             + ' ' + 'VHBLNP'
023200010601     C                   ELSE
023300010601     C                   MOVE      VHBLNP        VABLNP
023400010601     C     VABLNP        IFLE      *ZEROS
023500010601     C                   SETON                                        31
023600010601     C                   eval      vinmsg = %trimr(vinmsg)
023700010601     C                             + ' ' + 'VHBLNP'
023800010601     C                   ENDIF
023900010601     C                   ENDIF
024000010601     C*
024100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024200010601     C                   eval      depfil = VABLNP
024300010601     C                   exsr      repfil
024400010601     C                   if        depfil = invfil
024500021014     C                   if        vlrpoi = 999
024600021010     C                   MOVE(P)   invfil        VABFGS
024700021014     C                   else
024800021014     C                   MOVE(P)   vlrpoi        VABFGS
024900021014     C                   endif
025000010601     C*
025100010601     C                   exsr      PREELA
025200100125     C*
025300100125     C* Forzatura parametri cliente
025400100125     C                   EVAL      VABCCM = 1120551
025500100125     C                   EVAL      VABLNP = 112
025600100125     C                   EVAL      VABCTR = 000
025700100125     C                   EVAL      VABCBO = '1'
025800100125     C                   EVAL      VABCTM = *blanks
025900100125     C                   IF        vlrppt > *blanks
026000100125     C                   IF        %subst(vlrppt:1:7) <> *blanks
026100100125     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026200100125     C                   EXSR      CHKNUM
026300100125     C                   IF        PiInt=*on
026400100125     C                   Z-ADD     PiVal         VABCCM
026500100125     C                   ENDIF
026600100125     C                   ENDIF
026700100125     C                   IF        %subst(vlrppt:8:3) <> *blanks
026800100125     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026900100125     C                   EXSR      CHKNUM
027000100125     C                   IF        PiInt=*on
027100100125     C                   Z-ADD     PiVal         VABLNP
027200100125     C                   ENDIF
027300100125     C                   ENDIF
027400100125     C                   IF        %subst(vlrppt:11:3) <> *blanks
027500100125     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027600100125     C                   EXSR      CHKNUM
027700100125     C                   IF        PiInt=*on
027800100125     C                   Z-ADD     PiVal         VABCTR
027900100125     C                   ENDIF
028000100125     C                   ENDIF
028100100125     C                   ENDIF
028200100125     C*
028300100125     C                   movel(p)  datcor        VABAAS
028400100125     C* NSP
028500100125     C                   EVAL      PiStr=%trim(vhbNSP)
028600100125     C                   EXSR      CHKNUM
028700100125     C                   IF        PiInt=*on
028800100125     C                   Z-ADD     PiVal         VABNSP
028900100125     C                   ELSE
029000100125     C                   SETON                                        32
029100100125     C                   EVAL      vinmsg = %trimr(vinmsg)
029200100125     C                             + ' ' + 'VABRSP'
029300100125     C                   ENDIF
029400100125     C* DCR
029500100125     C                   IF        %trim(vhbDCR)<>*blanks
029600100125     C                             AND
029700100125     C                             %trim(vhbDCR)<>*zeros
029800100125     C                   EVAL      PiStr=%trim(vhbDCR)
029900100125     C                   EXSR      CHKNUM
030000100125     C                   IF        PiInt=*on
030100100125     C                   ADD       PiVal         VABDCR
030200100125     C                   ELSE
030300100125     C                   SETON                                        32
030400100125     C                   EVAL      vinmsg = %trimr(vinmsg)
030500100125     C                             + ' ' + 'VABDCR'
030600100125     C                   ENDIF
030700100125     C                   ENDIF
030800100125     C* RMN
030900100125     C                   EVAL      PiStr=%trim(vhbRMN)
031000100125     C                   EXSR      CHKNUM
031100100125     C                   IF        PiInt=*on
031200100125     C                   Z-ADD     PiVal         VABRMN
031300100125     C                   ELSE
031400100125     C                   SETON                                        32
031500100125     C                   EVAL      vinmsg = %trimr(vinmsg)
031600100125     C                             + ' ' + 'VABRMN'
031700100125     C                   ENDIF
031800100125     C* NCL
031900100125     C                   EVAL      PiStr=%trim(vhbNCL)
032000100125     C                   EXSR      CHKNUM
032100100125     C                   IF        PiInt=*on
032200100125     C                   ADD       PiVal         VABNCL
032300100125     C                   ELSE
032400100125     C                   SETON                                        32
032500100125     C                   EVAL      vinmsg = %trimr(vinmsg)
032600100125     C                             + ' ' + 'VABNCL'
032700100125     C                   ENDIF
032800100125     C* PKB
032900100125     C                   IF        %trim(vhbPKB)<>*blanks
033000100125     C                             AND
033100100125     C                             %trim(vhbPKB)<>*zeros
033200100125     C                             AND
033300100125     C                             %trim(vhbPKB)<>'0,0'
033400100125     C                             AND
033500100125     C                             %trim(vhbPKB)<>'0'
033600100125     C                   EVAL      PiStr=%trim(vhbPKB)
033700100125     C                   EXSR      CHKNUM
033800100125     C                   IF        PiNum=*on
033900100125     C                   ADD       PiVal         VABPKB
034000100125     C                   ELSE
034100100125     C                   SETON                                        32
034200100125     C                   EVAL      vinmsg = %trimr(vinmsg)
034300100125     C                             + ' ' + 'VABPKB'
034400100125     C                   ENDIF
034500100125     C                   ENDIF
034600100125     C* CAS
034700100125     C                   IF        %trim(vhbCAS)<>*blanks
034800100125     C                             AND
034900100125     C                             %trim(vhbCAS)<>*zeros
035000100125     C                             AND
035100100125     C                             %trim(vhbCAS)<>'0,0'
035200100125     C                             AND
035300100125     C                             %trim(vhbCAS)<>'0'
035400100125     C                   EVAL      PiStr=%trim(vhbCAS)
035500100125     C                   EXSR      CHKNUM
035600100125     C                   IF        PiNum=*on
035700100125     C                   ADD       PiVal         VABCAS
035800100125     C                   ELSE
035900100125     C                   SETON                                        32
036000100125     C                   EVAL      vinmsg = %trimr(vinmsg)
036100100125     C                             + ' ' + 'VABCAS'
036200100125     C                   ENDIF
036300100125     C                   ENDIF
036400121204     C* CTR
036500121204     C                   EVAL      PiStr=%trim(vhbCTR)
036600121204     C                   EXSR      CHKNUM
036700121204     C                   IF        PiInt=*on
036800121204     C                   ADD       PiVal         VABCTR
036900121204     C                   ELSE
037000121204     C                   SETON                                        32
037100121204     C                   EVAL      vinmsg = %trimr(vinmsg)
037200121204     C                             + ' ' + 'VABCTR'
037300121204     C                   ENDIF
037400971021     C*
037500971021     C* CAMPI ALFABETICI:
037600971021     C                   MOVE      VHBATB        VABATB
037700971021     C                   MOVE      VHBCBO        VABCBO
037800971021     C                   MOVE      VHBRSD        VABRSD
037900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
038100020117     C* ==
038200971021     C                   MOVE      VHBRD2        VABRD2
038300971021     C                   MOVE      VHBIND        VABIND
038400971021     C                   MOVE      VHBCAD        VABCAD
038500971021     C                   MOVE      VHBLOD        VABLOD
038600971021     C                   MOVE      VHBPRD        VABPRD
038700971021     C                   MOVE      VHBNZD        VABNZD
038800971021     C                   MOVE      VHBGC1        VABGC1
038900971021     C                   MOVE      VHBGC2        VABGC2
039000971021     C                   MOVE      VHBTSP        VABTSP
039100971021     C                   MOVE      VHBVAS        VABVAS
039200971021     C                   MOVE      VHBNAS        VABNAS
039300971021     C                   MOVE      VHBTIC        VABTIC
039400971021     C                   MOVE      VHBVCA        VABVCA
039500971021     C                   MOVE      VHBGCA        VABGCA
039600971021     C                   MOVE      VHBRMA        VABRMA
039700971021     C                   MOVE      VHBXCO        VABXCO
039800971021     C                   MOVE      VHBNOT        VABNOT
039900971021     C                   MOVE      VHBNT2        VABNT2
040000971021     C                   MOVE      VHBCTM        VABCTM
040100971021     C                   MOVE      VHBFFD        VABFFD
040200971021     C                   MOVE      VHBTCR        VABTCR
040300971021     C                   MOVE      VHBCTS        VABCTS
040400971021     C                   MOVE      VHBFTM        VABFTM
040500971021     C                   MOVE      VHBVAD        VABVAD
040600971021     C                   MOVE      VHBGMA        VABGMA
040700971021     C                   MOVE      VHBGGA        VABGGA
040800971021     C                   MOVE      VHBGVA        VABGVA
040900971021     C                   MOVE      VHBTC1        VABTC1
041000971021     C                   MOVE      VHBTC2        VABTC2
041100971021     C                   MOVE      VHBSCL        VABSCL
041200971021     C                   MOVE      VHBRMO        VABRMO
041300971021     C                   MOVE      VHBCMO        VABCMO
041400971021     C                   MOVE      VHBNMO        VABNMO
041500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
041600060329     C* NELLA NAZIONE MITTENTE ORIGINALE
041700060331     C                   IF        %trim(VHBNMO) < 'A'
041800060329     C                   MOVE      *BLANKS       VABNMO
041900060329     C                   ENDIF
042000050418     C*
042100050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
042200050418     C                   IF        %trim(VABNZD) = 'I'   OR
042300050418     C                             %trim(VABNZD) = 'IT'  OR
042400050418     C                             %trim(VABNZD) = 'ITA'
042500050418     C                   EVAL      VABNZD = *blanks
042600050418     C                   ENDIF
042700041025     C*
042800041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
042900041025     C                   IF        *IN52 = *ON
043000041025     C* Reperisco la provincia dal CAP e dalla località
043100041025     C                   IF        VABCAD <> *blanks AND
043200050907     C                             VABPRD  = *blanks AND
043300050907     C                             VABNZD  = *blanks
043400041025     C                   CLEAR                   TISI95DS
043500041025     C                   EVAL      I95TCN = '3'
043600041025     C                   Z-ADD     datcor        I95DAT
043700041025     C                   EVAL      I95CAP = VABCAD
043800041025     C                   EVAL      I95LOC = VABLOD
043900041025     C                   CALL      'TISI95R'
044000041025     C                   PARM                    TISI95DS
044100041025     C                   EVAL      VABPRD = O95PRV
044200041025     C                   ENDIF
044300041025     C                   ENDIF
044400040630     C*
044500040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
044600040630     C                   exsr      exeBon
044700100125     C*
044800100125     C* Verifica valori x ulteriori eventuali "forzature"
044900100125     C                   if        vabRMA = *blanks
045000100125     C                   movel(p)  vabRMN        vabRMA
045100100125     C                   endif
045200930409     C*
045300010604     C  N31              ADD       1             §CTROK            7 0
045400010604     C   32              ADD       1             §CTRMO            7 0
045500010604     C   31              ADD       1             §CTRNO            7 0
045600020916     C  N31              WRITE     FIVAB000
045700990910     C*
045800010601     C                   if        *in31 = *off and
045900010601     C                             *in32 = *off
046000990910     C                   eval      vinflg = '1'
046100990910     C                   else
046200990910     C                   eval      vinflg = '2'
046300990910     C                   endif
046400000905     C                   endif
046500000905     C                   endif
046600010601     C*
046700010601     C                   endif
046800000905     C*
046900000905     C                   else
047000000905     C                   eval      vinflg = '1'
047100000905     C                   endif
047200000905     C*
047300000905     C  N70              update    tivin000
047400000905     C*
047500991022     C  N70              ENDdo
047600010601     C
047700010601     C*
047800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
047900010601     C                   if        cntNonEl = *zeros or
048000010601     C                             flgMulti = '0'
048100990910     C* Se non ci sono record con errori ...
048200010601     C                   if        §ctrno = 0 and
048300010601     C                             §ctrmo = 0 and
048400010601     C                             flgStato <> '2'
048500990910     C* ... restituisco esito OK.
048600990921     C                   eval      wrkesito = '0'
048700990910     C                   else
048800010601     C                   if        §ctrok > 0
048900000710     C                   eval      wrkesito = '1'
049000000710     C                   else
049100010615     C                   if        flgOk = '0'
049200010615     C                   eval      wrkesito = '2'
049300010615     C                   else
049400010615     C                   eval      wrkesito = '6'
049500010615     C                   endif
049600000710     C                   endif
049700010615     C                   endif
049800010601     C                   else
049900010601     C                   eval      wrkesito = '9'
050000010601     C                   endif
050100990910     C*
050200990914     C                   if        %open(tivin00r)
050300990908     C                   close     tivin00r
050400990914     C                   endif
050500020916     C                   if        %open(fivabwwr)
050600020916     C                   close     fivabwwr
050700990914     C                   endif
050800010601     C*
050900010601     C                   if        vlrpoi <> 999
051000010601     C                   eval      invfil = vlrpoi
051100010601     C                   endif
051200990910     C*
051300990920     C                   if        §ctrok > 0
051400010601     C                             and invfil > *zeros
051500990920     C                   exsr      invio
051600990920     C                   endif
051700010612     C*
051800010612     C                   if        flgGiro = '1'
051900010612     C                   exsr      endela
052000010612     C                   endif
052100990920     C*
052200910830     C                   ENDSR
052300000613     C***
052400990920
052500010601
052600010601
052700010601
052800010601
052900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053000010601     C     repfil        BEGSR
053100010601     C*
053200010601     C                   if        invfil = *zeros and
053300010601     C                             depfil > *zeros and
053400010613     C                             (vinflg = *blanks or
053500010613     C                              vinflg = *zeros)
053600010601     C
053700010601     C                   eval      invfil = depfil
053800010601     C                   endif
053900010601     C*
054000010601     C                   if        depfil <> invfil and
054100010601     C                             invfil > *zeros
054200010601     C                   eval      flgMulti = '1'
054300010601     C                   if        vinflg = *blanks
054400010601     C                   add       1             cntNonEl
054500010601     C                   endif
054600010601     C                   endif
054700010601     C*
054800010601     C                   if        vinflg = '2'
054900010601     C                   eval      flgStato = '2'
055000010601     C                   endif
055100010601     C*
055200010601     C                   ENDSR
055300010601     C***
055400010601
055500010601
055600010601
055700010601
055800010601
055900010601
056000010601
056100990920      /TITLE Invio dei dati al punto operativo.
056200000613     C     invio         BEGSR
056300990920     C*
056400990920     C                   reset                   dscmz
056500010601     C                   move      invfil        cmzdst
056600990920     C                   eval      cmzfld = vlrfou
056700990920     C                   eval      cmzmbd = vlrhdl
056800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056900000710     C                   if        prmfir = *blanks
057000020916     C                   eval      cmzfla = 'FIVAB00F'
057100020916     C                   eval      cmzmba = 'FIVAB00F'
057200000710     C                   else
057300000710     C                   eval      cmzfla = prmfir
057400000710     C                   eval      cmzmba = prmfir
057500000710     C                   endif
057600990920     C                   eval      cmznrr = *zeros
057700990920     C                   move      §ctrok        cmznrr
057800021003     C                   eval      cmzlba = vlrfl1
057900990920     C                   call(e)   'TIS711C'
058000990920     C                   parm                    dscmz
058100990921     C                   parm      *blanks       esito
058200990920     C                   if        %error
058300990920     C                             or cmzerr = '1'
058400990921     C                             or esito  = '1'
058500000710     C                   eval      wrkesito = '3'
058600990920     C                   endif
058700990920     C*
058800000613     C                   ENDSR
058900010601
059000010601
059100010601
059200010601
059300010601      /TITLE Invio dei dati al punto operativo.
059400010601     C     opeini        BEGSR
059500010601     C*
059600010601     C* Inizializzo flag e contatori operativi
059700010601     C                   movel     '0'           flgGiro           1
059800010601     C                   movel     '0'           flgMulti          1
059900010601     C                   movel     '1'           flgStato          1
060000010615     C                   movel     '0'           flgOk             1
060100010601     C                   z-add     *zeros        cntNonEl         10 0
060200010601     C                   z-add     *zeros        depfil            3 0
060300010601     C                   z-add     *zeros        invfil            3 0
060400030407     C*
060500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
060600030407     C                   if        %subst(vlrppt:1:1) = 'S'
060700030407     C                   seton                                        50
060800030407     C                   else
060900030407     C                   setoff                                       50
061000030407     C                   endif
061100041001     C*
061200041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
061300041001     C                   if        %subst(vlrppt:2:1) = 'S'
061400041001     C                   seton                                        51
061500041001     C                   else
061600041001     C                   setoff                                       51
061700041001     C                   endif
061800041025     C*
061900041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
062000041025     C* (naturalmente solo se nn già presente)
062100041025     C                   if        %subst(vlrppt:3:1) = 'S'
062200041025     C                   seton                                        52
062300041025     C                   else
062400041025     C                   setoff                                       52
062500041025     C                   endif
062600010601     C*
062700010601     C                   ENDSR
062800010601     C***
062900040630
063000040630
063100040630
063200040630
063300040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
063400040630     C     exeBon        BEGSR
063500040630     C*
063600040630     C* Imposto i parametri x il *pgm d "bonifica"
063700040630     C                   EVAL      ElencoChar = *blanks
063800040630     C                   EVAL      TipoElenco = *blanks
063900040630     C                   EVAL      CharSost   = *blanks
064000040630     C                   EVAL      UpperCase  = '1'
064100040630     C                   EVAL      ChkNull    = '1'
064200040630     C                   EVAL      CharNull   = *blanks
064300040630     C*
064400040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
064500040630     C                   EVAL      TxtInOut   = VABRSD
064600040630     C                   EVAL      EsitoBon   = *blanks
064700040630     C*
064800040630     C                   CALL      'XCHKCHAR'
064900040630     C                   PARM                    TxtInOut
065000040630     C                   PARM                    ElencoChar
065100040630     C                   PARM                    TipoElenco
065200040630     C                   PARM                    CharSost
065300040630     C                   PARM                    UpperCase
065400040630     C                   PARM                    ChkNull
065500040630     C                   PARM                    CharNull
065600040630     C                   PARM                    EsitoBon
065700040630     C*
065800040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
065900040630     C                   IF        EsitoBon = '1'
066000040630     C                   EVAL      VABRSD = TxtInOut
066100040630     C                   ENDIF
066200040630     C*
066300040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
066400040630     C                   EVAL      TxtInOut   = VABIND
066500040630     C                   EVAL      EsitoBon   = *blanks
066600040630     C*
066700040630     C                   CALL      'XCHKCHAR'
066800040630     C                   PARM                    TxtInOut
066900040630     C                   PARM                    ElencoChar
067000040630     C                   PARM                    TipoElenco
067100040630     C                   PARM                    CharSost
067200040630     C                   PARM                    UpperCase
067300040630     C                   PARM                    ChkNull
067400040630     C                   PARM                    CharNull
067500040630     C                   PARM                    EsitoBon
067600040630     C*
067700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
067800040630     C                   IF        EsitoBon = '1'
067900040630     C                   EVAL      VABIND = TxtInOut
068000040630     C                   ENDIF
068100040630     C*
068200040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
068300040630     C                   EVAL      TxtInOut   = VABLOD
068400040630     C                   EVAL      EsitoBon   = *blanks
068500040630     C*
068600040630     C                   CALL      'XCHKCHAR'
068700040630     C                   PARM                    TxtInOut
068800040630     C                   PARM                    ElencoChar
068900040630     C                   PARM                    TipoElenco
069000040630     C                   PARM                    CharSost
069100040630     C                   PARM                    UpperCase
069200040630     C                   PARM                    ChkNull
069300040630     C                   PARM                    CharNull
069400040630     C                   PARM                    EsitoBon
069500040630     C*
069600040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
069700040630     C                   IF        EsitoBon = '1'
069800040630     C                   EVAL      VABLOD = TxtInOut
069900040630     C                   ENDIF
070000040630     C*
070100040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
070200040630     C                   EVAL      TxtInOut   = VABNOT
070300040630     C                   EVAL      EsitoBon   = *blanks
070400040630     C                   EVAL      UpperCase  = *blanks
070500040630     C*
070600040630     C                   CALL      'XCHKCHAR'
070700040630     C                   PARM                    TxtInOut
070800040630     C                   PARM                    ElencoChar
070900040630     C                   PARM                    TipoElenco
071000040630     C                   PARM                    CharSost
071100040630     C                   PARM                    UpperCase
071200040630     C                   PARM                    ChkNull
071300040630     C                   PARM                    CharNull
071400040630     C                   PARM                    EsitoBon
071500040630     C*
071600040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
071700040630     C                   IF        EsitoBon = '1'
071800040630     C                   EVAL      VABNOT = TxtInOut
071900040630     C                   ENDIF
072000040630     C*
072100040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
072200040630     C                   EVAL      TxtInOut   = VABNT2
072300040630     C                   EVAL      EsitoBon   = *blanks
072400040630     C                   EVAL      UpperCase  = *blanks
072500040630     C*
072600040630     C                   CALL      'XCHKCHAR'
072700040630     C                   PARM                    TxtInOut
072800040630     C                   PARM                    ElencoChar
072900040630     C                   PARM                    TipoElenco
073000040630     C                   PARM                    CharSost
073100040630     C                   PARM                    UpperCase
073200040630     C                   PARM                    ChkNull
073300040630     C                   PARM                    CharNull
073400040630     C                   PARM                    EsitoBon
073500040630     C*
073600040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
073700040630     C                   IF        EsitoBon = '1'
073800040630     C                   EVAL      VABNT2 = TxtInOut
073900040630     C                   ENDIF
074000040630     C*
074100040630     C                   ENDSR
074200040630
074300010601
074400100125     C*----------------------------------------------------*
074500100125     C*  CONTROLLO NUMERICITA' CAMPI
074600100125     C*----------------------------------------------------*
074700100125     C     CHKNUM        BEGSR
074800100125     C*
074900100125     C                   IF        PiDecChr = *blanks
075000100125     C                   EVAL      PiDecChr = ','
075100100125     C                   ENDIF
075200100125     C*
075300100125     C                   callp(e)  UBISNUM_Check(PiStr
075400100125     C                                          :PiDecChr
075500100125     C                                          :PiVal
075600100125     C                                          :PiNum
075700100125     C                                          :PiInt)
075800100125     C*
075900100125     C                   IF        %error
076000100125     C                   EVAL      PiInt=*off
076100100125     C                   ENDIF
076200100125     C*
076300100125     C                   ENDSR
076400100125     C***
076500010601
076600990910
076700000613     C     *inzsr        BEGSR
076800990910     C*
076900990910     C     *entry        plist
077000990920     C                   parm                    tivlrds
077100990921     C                   parm      wrkesito      esito
077200000724     C                   parm                    prmlit
077300000710     C                   parm                    prmfir
077400010606     C*
077500010606     C* CALCOLA LA DATA CORRENTE
077600100125     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077700100125     C                   eval      datcor = %dec(%date() : *ISO)
077800000613     C*
077900000613     C                   ENDSR
078000000613     C***
