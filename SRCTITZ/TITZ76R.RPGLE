000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070719     D tisi95ds      e ds
001600130325     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001700130325     D* 10+3
001800130325     D                 DS
001900130325     D  WI15                   1     10
002000130325     D  WD15                  11     13
002100130325     D  WASC15                 1     13
002200130325     D*  6+1
002300130325     D                 DS
002400130325     D  WI09                   1      6
002500130325     D  WD09                   7      7
002600130325     D  WASC09                 1      7
002700130325     D*  2+3
002800130325     D                 DS
002900130325     D  WI07                   1      2
003000130325     D  WD07                   3      5
003100130325     D  WASC07                 1      5
003200130325     D*
003300990910     D esito           s              1
003400000724     D prmlit          s             10
003500000710     D prmfir          s             10
003600990921     D wrkesito        s                   like(esito)
003700000613     D rrnum           s              6  0 INZ(*zeros)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200130104     D w70             s             70    inz(*blanks)
004300130325     D*
004400130325     D TxtInOut        S           2048
004500130325     D ElencoChar      S            256
004600130325     D TipoElenco      S              1
004700130325     D CharSost        S              1
004800130325     D UpperCase       S              1
004900130325     D ChkNull         S              1
005000130325     D CharNull        S              1
005100130325     D EsitoBon        S              1
005200000830
005300130325     D*---------------------------------------------------------
005400130325     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
005500130325     D*---------------------------------------------------------
005600130325     D                 ds
005700130325     D vhbatb                  1      1
005800130325     D vhbccm                  2      8
005900130325     D vhblnp                  9     11
006000130325     D vhbaas                 12     15
006100130325     D vhbmgs                 16     19
006200130325     D vhbnrs                 20     21
006300130325     D vhbnsp                 22     28
006400130325     D vhbcbo                 29     30
006500130325     D vhblna                 31     33
006600130325     D vhbrsd                 34     68
006700130325     D vhbrd2                 69    103
006800130325     D vhbind                104    138
006900130325     D vhbcad                139    147
007000130325     D vhblod                148    182
007100130325     D vhbprd                183    184
007200130325     D vhbnzd                185    187
007300130325     D vhbgc1                188    189
007400130325     D vhbgc2                190    191
007500130325     D vhbctr                192    194
007600130325     D vhbtsp                195    195
007700130325     D vhbias                196    208
007800130325     D vhbvas                209    211
007900130325     D vhbnas                212    226
008000130325     D vhbncl                227    231
008100130325     D vhbpkb                232    238
008200130325     D vhbvlb                239    243
008300130325     D vhbqft                244    256
008400130325     D vhbcas                257    269
008500130325     D vhbtic                270    271
008600130325     D vhbvca                272    274
008700130325     D vhbgca                275    276
008800130325     D vhbrmn                277    291
008900130325     D vhbrma                292    306
009000130325     D vhbncd                307    313
009100130325     D vhbnca                314    320
009200130325     D vhbxco                321    321
009300130325     D vhbnot                322    356
009400130325     D vhbnt2                357    391
009500130325     D vhbznc                392    393
009600130325     D vhbctm                394    395
009700130325     D vhbffd                396    396
009800130325     D vhbdcr                397    404
009900130325     D vhbtcr                405    405
010000130325     D vhbhcr                406    409
010100130325     D vhbcts                410    411
010200130325     D vhbftm                412    412
010300130325     D vhbvmd                413    425
010400130325     D vhbvad                426    428
010500130325     D vhbgma                429    430
010600130325     D vhbgga                431    432
010700130325     D vhbgva                433    434
010800130325     D vhbtc1                435    435
010900130325     D vhbtc2                436    436
011000130325     D vhbscl                437    437
011100130325     D vhbant                438    446
011200130325     D vhbrmo                447    471
011300130325     D vhbcmo                472    480
011400130325     D vhbnmo                481    483
011500130325     D  X_rec                  1    483    inz
011600130325     D*
011700130325     D fnvataar        ds                  inz
011800130325     D  a_vatatb               1      1
011900130325     D  a_vatccm               2      8
012000130325     D  a_vatlnp               9     11
012100130325     D  a_vataas              12     15
012200130325     D  a_vatnrs              16     17
012300130325     D  a_vatnsp              18     24
012400130325     D  a_vattrc              25     25
012500130325     D  a_vatnot              26     60
012600041025     D*------------------
012700041025     D* DS REPERIMENTO NUMERATORE
012800041025     D*------------------
012900041025     D trul33ds      e ds                  inz
013000041025     D*------------------
013100041025     D* DS ARCHITETTURA
013200041025     D*------------------
013300041025     D kpjba         e ds                  inz
013400041025     D*------------------
013500990908
013600010201
013700010201
013800000913     C                   reset                   rrnum
013900990921     C                   reset                   esito
014000990921     C                   reset                   wrkesito
014100000613     C*
014200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
014300070719     C*
014400070719     C* Esegue lancio TISI95R solo x chiusura
014500070719     C                   CLEAR                   TISI95DS
014600070719     C                   EVAL      I95TLA = 'C'
014700070719     C                   CALL      'TISI95R'
014800070719     C                   PARM                    TISI95DS
014900000613     C*
015000010202     C* Effettuo la chiamata al CLLE preposto
015100040506     C                   call(e)   'TITVVTC'
015200010202     C                   parm                    parccm
015300010202     C                   parm                    parmbr
015400010202     C                   parm      '2'           paropz
015500000616     C*
015600010201     C                   seton                                        LR
015700990908
015800000801
015900910830     C*--------------------------------------------------------
016000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016100910830     C*--------------------------------------------------------
016200040526     C     RWFILE        BEGSR
016300990910     C*
016400990914     C                   if        not %open(tivin00r)
016500990908     C                   open      tivin00r
016600990914     C                   endif
016700021113     C                   if        not %open(fivabwwr)
016800021113     C                   open      fivabwwr
016900990914     C                   endif
017000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017100020305     C                   exsr      prevat
017200010201     C*
017300010202     C                   if        chkcall = '0'
017400010202     C*
017500021113     C                   if        not %open(fivatwwr)
017600021113     C                   open      fivatwwr
017700010201     C                   endif
017800990910     C*
017900010201     C                   clear                   §CTROKVB          5 0
018000020305     C                   clear                   §CTROKVT          5 0
018100000801     C                   clear                   §CTRMO            5 0
018200000801     C                   clear                   §CTRNO            5 0
018300100729     C*
018400100729     C                   z-add     1             wGiro             1 0
018500040910     C*
018600921023     C                   DO        *HIVAL
018700990913     C*
018800990915     C                   READ      tivin00r                               70
018900040910     C                   if        vindta > *blanks
019000000613     C                   add       1             rrnum
019100000801     C*
019200000801     C                   if        *in70 = *off
019300000801     C                             and
019400000801     C                             (vinflg = *blanks
019500000801     C                              or vinflg = '0'
019600000801     C                              or vinflg = '2')
019700000801     C*
019800000801     C                   clear                   vinmsg
019900000801     C                   eval      vinflg = '1'
020000040910     C*
020100040910     C* Eseguo routine d traduzione
020200040910     C                   exsr      impvabvat
020300040802     C*
020400010305     C                   endif
020500000905     C*
020600000905     C                   else
020700000905     C                   eval      vinflg = '1'
020800000905     C                   endif
020900000905     C*
021000000905     C  N70              update    tivin000
021100000905     C*
021200991022     C  N70              ENDdo
021300100722     C*
021400100722     C* Scarico i buffer testata ancora "in canna"
021500100722     C                   WRITE     FIVAB000
021600010202     C*
021700010202     C                   endif
021800990910
021900990910     C* Se non ci sono record con errori ...
022000000710     C                   if        §ctrno = 0
022100990910     C* ... restituisco esito OK.
022200990921     C                   eval      wrkesito = '0'
022300990910     C                   else
022400010201     C                   if        §ctrokvb > 0
022500990921     C                   eval      wrkesito = '1'
022600000710     C                   else
022700000710     C                   eval      wrkesito = '2'
022800990910     C                   endif
022900000710     C                   endif
023000990910     C*
023100990914     C                   if        %open(tivin00r)
023200990908     C                   close     tivin00r
023300990914     C                   endif
023400021113     C                   if        %open(fivabwwr)
023500021113     C                   close     fivabwwr
023600990914     C                   endif
023700021113     C                   if        %open(fivatwwr)
023800021113     C                   close     fivatwwr
023900010201     C                   endif
024000990910     C*
024100010201     C                   if        §ctrokvb > 0
024200000724     C                             and vlrpoi <> *zeros
024300010202     C                   exsr      invio
024400990920     C                   endif
024500990920     C*
024600910830     C                   ENDSR
024700000613     C***
024800990920
024900000801     C*----------------------------------------------------*
025000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025100000801     C*----------------------------------------------------*
025200010201     C     INZVAR        BEGSR
025300000801     C*
025400040802     C                   Z-ADD     *zeros        Num5_0            5 0
025500040802     C                   MOVEL     '0'           FlgCAS            1
025600000801     C*
025700000801     C                   ENDSR
025800000801     C*----------------------------------------------------*
025900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
026000000801     C*----------------------------------------------------*
026100000801     C     DEFCAM        BEGSR
026200000801     C*
026300021113     C                   CLEAR                   FIVAB000
026400040802     C                   CLEAR                   FIVAT000
026500130326     C*
026600130326     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
026700130326     C                   if        %subst(vlrppt:2:1) = 'S'
026800130326     C                   seton                                        51
026900130326     C                   else
027000130326     C                   setoff                                       51
027100130326     C                   endif
027200130326     C*
027300130326     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
027400130326     C* (naturalmente solo se nn già presente)
027500130326     C                   if        %subst(vlrppt:3:1) = 'S'
027600130326     C                   seton                                        52
027700130326     C                   else
027800130326     C                   setoff                                       52
027900130326     C                   endif
028000130326     C*
028100130326     C* RICEVENDO NEL FILE TUTTI I DATI, QUESTA PARTE NON HA SENSO
028200130326     C* si esce dalla routine, ma lascio le istruzioni perché potrebbero venire buone un domani
028300130326     C                   LEAVESR
028400130326     C*
028500020619     C* Imposto i valori di default...
028600130325     C                   Z-ADD     2552663       VABCCM
028700130325     C                   Z-ADD     2552663       VATCCM
028800130325     C                   Z-ADD     255           VABLNP
028900130325     C                   Z-ADD     255           VATLNP
029000070531     C                   Z-ADD     000           VABCTR
029100040823     C                   MOVEL     '1'           VABCBO
029200020619     C* ... e poi verifico se sono stati passati come parametri
029300020619     C                   IF        vlrppt > *blanks
029400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABCCM
029900020619     C                   Z-ADD     PiVal         VATCCM
030000020619     C                   ENDIF
030100040506     C                   ENDIF
030200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABLNP
030700020619     C                   Z-ADD     PiVal         VATLNP
030800040506     C                   ENDIF
030900020619     C                   ENDIF
031000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200020619     C                   EXSR      CHKNUM
031300020619     C                   IF        PiInt=*on
031400020619     C                   Z-ADD     PiVal         VABCTR
031500040506     C                   ENDIF
031600020619     C                   ENDIF
031700060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
031800060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
031900060202     C                   ENDIF
032000020619     C                   ENDIF
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
032500000801     C*----------------------------------------------------*
032600040910     C     IMPVABVAT     BEGSR
032700040910     C*
032800040910     C* Traduzione relativa ai tipi record del file del cliente
032900040910     C*
033000071210     C*
033100071210     C***
033200130325     C* ...tipo record 'T' (testata documento)
033300130325     C                   IF        %subst(vindta:1:1) = 'T'
033400100722     C*
033500100722     C* Se nn primo giro => scarico il buffer precedente
033600100722     C                   if        wGiro = 1
033700100722     C                   eval      wGiro = 2
033800100722     C                   else
033900100722     C                   WRITE     FIVAB000
034000100722     C                   endif
034100100722     C*
034200071210     C* Resetto indicatore d anomalia sul singolo record
034300071210     C                   eval      vinflg = '1'
034400071210     C* ......inizializzazioni iniziali e formati record file Bartolini
034500071210     C                   EXSR      INZVAR
034600071210     C                   EXSR      DEFCAM
034700071210     C*
034800071210     C                   Z-ADD     *zeros        errore            1 0
034900130325     C*
035000130325     C* ogni rcd di testata letto scarico un NSP
035100130325     C                   exsr      repNSP
035200130325     C*
035300130325     C* gestisco i dati come da standard VAB in fmt EBCDIC
035400130325     C                   EXSR      VABStdEBCDIC
035500130325     C*
035600130325     C                   MOVE(P)   vlrpoi        VABFGS
035700130325     C                   MOVE(P)   vlrpoi        VATFGS
035800060202     C*
035900060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036000060202     C                   IF        FlgCAS <> '0'
036100060202     C                   IF        VABCBO = '1'
036200060202     C                   EVAL      VABCBO = '4'
036300060202     C                   ELSE
036400060202     C                   EVAL      VABCBO = '6'
036500060202     C                   ENDIF
036600060202     C                   ENDIF
036700060202     C*
036800060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036900060202     C                   EXSR      CHKIMPDIV
037000130326     C*
037100130326     C* per equipararci alla routine VAB/VATStdEBCDIC copiata da TIST4/T9R1
037200130326     C* e che usa gli indicatori,
037300130326     C* valorizzo errore a seconda degli indicatori
037400130326     C                   IF        *in31 = *on
037500130326     C                   EVAL      errore = 1
037600130326     C                   ENDIF
037700130326     C*
037800130326     C                   IF        errore <> *zeros
037900130326     C                   ADD       1             §CTRNO
038000130326     C                   EVAL      vinflg = '2'
038100130326     C                   ELSE
038200130326     C                   ADD       1             §CTROKVB
038300130326     C                   ENDIF
038400040910     C*
038500040910     C                   ENDIF
038600110425     C***
038700130325     C* ...tipo record 'D' (dettaglio)
038800130325     C                   IF        %subst(vindta:1:1) = 'D'
038900130325     C*
039000130325     C                   eval      fnvataar = vindta
039100130325     C*
039200070719     C* Resetto indicatore d anomalia sul singolo record
039300070719     C                   eval      vinflg = '1'
039400130325     C*
039500130325     C* gestisco i dati come da standard VAT in fmt EBCDIC
039600130326     C                   EXSR      VATStdEBCDIC
039700130325     C*
039800130325     C                   IF        VATNOT <> *blanks
039900130325     C                   WRITE     FIVAT000
040000130325     C                   ENDIF
040100130326     C*
040200130326     C* per equipararci alla routine VAB/VATStdEBCDIC copiata da TIST4/T9R1
040300130326     C* e che usa gli indicatori,
040400130326     C* valorizzo errore a seconda degli indicatori
040500130326     C                   IF        *in31 = *on
040600130326     C                   EVAL      errore = 1
040700130326     C                   ENDIF
040800130326     C*
040900130326     C                   IF        errore <> *zeros
041000130326     C                   ADD       1             §CTRNO
041100130326     C                   EVAL      vinflg = '2'
041200130326     C                   ELSE
041300130326     C                   ADD       1             §CTROKVT
041400130326     C                   ENDIF
041500100722     C*
041600100722     C                   ENDIF
041700130326     C*
041800000801     C* Ebbene...
041900130326     C                   ADD       1             §CTRMO                         flag che non serve
042000000801     C*
042100000801     C                   ENDSR
042200130325     C*----------------------------------------------------*
042300130325     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
042400130325     C*----------------------------------------------------*
042500130325     C     REPNSP        BEGSR
042600130325     C*
042700130325     C                   SETON                                        60
042800130325     C*
042900130325     C* NSP => Stacco un numeratore da AZNUM
043000130325     C                   clear                   TRUL33DS
043100130325     C                   eval      I33OPE = *zeros
043200130325     C                   eval      I33CNU = 302
043300130325     C                   eval      I33NUM = 1
043400130325     C                   movel     TRUL33DS      KPJBU
043500130325     C                   call      'TRUL33R'
043600130325     C                   parm                    KPJBA
043700130325     C                   movel     KPJBU         TRUL33DS
043800130325     C                   if        O33ERR = *zeros
043900130325     C                   z-add     O33NRF        VABNSP
044000130325     C                   z-add     O33NRF        VATNSP
044100130325     C                   else
044200130325     C                   SETON                                        31
044300130325     C                   EVAL      vinmsg = %trimr(vinmsg)
044400130325     C                             + ' ' + 'VABNSP VATNSP'
044500130325     C                   endif
044600130325     C*
044700130325     C                   ENDSR
044800130325     C*--------------------------------------------------------
044900130325     C* VAB standard EBCDIC
045000130325     C*--------------------------------------------------------
045100130325     C     VABStdEBCDIC  BEGSR
045200130326     C*
045300130326     C                   eval      x_rec  = vindta
045400130325     C*
045500130325     C                   testb     x'3F'         vhbatb                   02
045600130325     C                   if        not *in02
045700130325     C*
045800130325     C                   SETOFF                                       303132
045900130325     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
046000130325     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
046100130325     C*    OPPURE TUTTI BLANKS
046200130325     C*
046300130325     C                   TESTN                   VHBLNP               30
046400130325     C  N30              TESTN                   VHBLNP                 30
046500130325     C  N30              TESTN                   VHBLNP                   30
046600130325     C                   MOVE      VHBLNP        W001A             1
046700130325     C     *IN30         IFEQ      *OFF
046800130325     C     W001A         ORLT      '0'
046900130325     C                   SETON                                        31
047000130325     C                   eval      vinmsg = %trimr(vinmsg)
047100130325     C                             + ' ' + 'VHBLNP'
047200130325     C                   ELSE
047300130325     C                   MOVE      VHBLNP        VABLNP
047400130325     C     VABLNP        IFLE      *ZEROS
047500130325     C                   SETON                                        31
047600130325     C                   eval      vinmsg = %trimr(vinmsg)
047700130325     C                             + ' ' + 'VHBLNP'
047800130325     C                   ENDIF
047900130325     C                   ENDIF
048000130325     C***
048100130326     C* viene gestiro in altro modo, fuori da questa routine
048200130326     C***
048300130325     C**Effettuo considerazioni x elaborazioni "multi-filiale"
048400130325     C***                eval      depfil = VABLNP
048500130325     C***                exsr      repfil
048600130325     C***                if        depfil = invfil
048700130325     C***                if        vlrpoi = 999
048800130325     C***                MOVE(P)   invfil        VABFGS
048900130325     C***                else
049000130325     C***                MOVE(P)   vlrpoi        VABFGS
049100130325     C***                endif
049200130325     C***
049300130325     C***                exsr      PREELA
049400130325     C*
049500130325     C                   TESTN                   VHBCCM               30
049600130325     C  N30              TESTN                   VHBCCM                 30
049700130325     C  N30              TESTN                   VHBCCM                   30
049800130325     C                   MOVE      VHBCCM        W001A             1
049900130325     C     *IN30         IFEQ      *OFF
050000130325     C     W001A         ORLT      '0'
050100130325     C                   SETON                                        31
050200130325     C                   eval      vinmsg = %trimr(vinmsg)
050300130325     C                             + ' ' + 'VHBCCM'
050400130325     C                   ELSE
050500130325     C                   MOVE      VHBCCM        VABCCM
050600130325     C     VABCCM        IFLE      *ZEROS
050700130325     C                   SETON                                        31
050800130325     C                   eval      vinmsg = %trimr(vinmsg)
050900130325     C                             + ' ' + 'VHBCCM'
051000130325     C                   ENDIF
051100130325     C                   ENDIF
051200130325     C*
051300130325     C                   TESTN                   VHBAAS               30
051400130325     C  N30              TESTN                   VHBAAS                 30
051500130325     C  N30              TESTN                   VHBAAS                   30
051600130325     C                   MOVE      VHBAAS        W001A             1
051700130325     C     *IN30         IFEQ      *OFF
051800130325     C     W001A         ORLT      '0'
051900130325     C     W001A         ANDNE     *BLANK
052000130325     C                   SETON                                        32
052100130325     C                   MOVEL     datcor        VABAAS
052200130325     C                   eval      vinmsg = %trimr(vinmsg)
052300130325     C                             + ' ' + 'VHBAAS'
052400130325     C                   ELSE
052500130325     C                   MOVE      VHBAAS        VABAAS
052600130325     C                   ENDIF
052700130325     C*
052800130325     C                   TESTN                   VHBMGS               30
052900130325     C  N30              TESTN                   VHBMGS                 30
053000130325     C  N30              TESTN                   VHBMGS                   30
053100130325     C                   MOVE      VHBMGS        W001A             1
053200130325     C     *IN30         IFEQ      *OFF
053300130325     C     W001A         ORLT      '0'
053400130325     C     W001A         ANDNE     *BLANK
053500130325     C                   SETON                                        32
053600130325     C                   MOVE      '0101'        VABMGS
053700130325     C                   eval      vinmsg = %trimr(vinmsg)
053800130325     C                             + ' ' + 'VHBMGS'
053900130325     C                   ELSE
054000130325     C                   MOVE      VHBMGS        VABMGS
054100130325     C                   ENDIF
054200130325     C*
054300130325     C                   TESTN                   VHBNRS               30
054400130325     C  N30              TESTN                   VHBNRS                 30
054500130325     C  N30              TESTN                   VHBNRS                   30
054600130325     C                   MOVE      VHBNRS        W001A             1
054700130325     C     *IN30         IFEQ      *OFF
054800130325     C     W001A         ORLT      '0'
054900130325     C     W001A         ANDNE     *BLANK
055000130325     C                   SETON                                        32
055100130325     C                   MOVE      *ZERO         VABNRS
055200130325     C                   eval      vinmsg = %trimr(vinmsg)
055300130325     C                             + ' ' + 'VHBNRS'
055400130325     C                   ELSE
055500130325     C                   MOVE      VHBNRS        VABNRS
055600130325     C                   ENDIF
055700130325     C***
055800130326     C* siccome reperisco il nr.spedizione da trul33, non devo prenderlo da file
055900130325     C***                TESTN                   VHBNSP               30
056000130325     C**N30              TESTN                   VHBNSP                 30
056100130325     C**N30              TESTN                   VHBNSP                   30
056200130325     C***                MOVE      VHBNSP        W001A             1
056300130325     C***  *IN30         IFEQ      *OFF
056400130325     C***  W001A         ORLT      '0'
056500130325     C***  W001A         ANDNE     *BLANK
056600130325     C***                SETON                                        32
056700130325     C***                MOVE      *ZERO         VABNSP
056800130325     C***                eval      vinmsg = %trimr(vinmsg)
056900130325     C***                          + ' ' + 'VHBNSP'
057000130325     C***                ELSE
057100130325     C***                MOVE      VHBNSP        VABNSP
057200130325     C***                ENDIF
057300130325     C*
057400130325     C                   TESTN                   VHBLNA               30
057500130325     C  N30              TESTN                   VHBLNA                 30
057600130325     C  N30              TESTN                   VHBLNA                   30
057700130325     C                   MOVE      VHBLNA        W001A             1
057800130325     C     *IN30         IFEQ      *OFF
057900130325     C     W001A         ORLT      '0'
058000130325     C     W001A         ANDNE     *BLANK
058100130325     C                   SETON                                        32
058200130325     C                   MOVE      *ZERO         VABLNA
058300130325     C                   eval      vinmsg = %trimr(vinmsg)
058400130325     C                             + ' ' + 'VHBLNA'
058500130325     C                   ELSE
058600130325     C                   MOVE      VHBLNA        VABLNA
058700130325     C                   ENDIF
058800130325     C*
058900130325     C                   TESTN                   VHBCTR               30
059000130325     C  N30              TESTN                   VHBCTR                 30
059100130325     C  N30              TESTN                   VHBCTR                   30
059200130325     C                   MOVE      VHBCTR        W001A             1
059300130325     C     *IN30         IFEQ      *OFF
059400130325     C     W001A         ORLT      '0'
059500130325     C     W001A         ANDNE     *BLANK
059600130325     C                   SETON                                        32
059700130325     C                   MOVE      *ZERO         VABCTR
059800130325     C                   eval      vinmsg = %trimr(vinmsg)
059900130325     C                             + ' ' + 'VHBCTR'
060000130325     C                   ELSE
060100130325     C                   MOVE      VHBCTR        VABCTR
060200130325     C                   ENDIF
060300130325     C*
060400130325     C                   MOVE      *ZERO         VABIAS
060500130325     C                   MOVE      VHBIAS        WASC15
060600130325     C                   TESTN                   WI15                 30
060700130325     C  N30              TESTN                   WI15                   30
060800130325     C  N30              TESTN                   WI15                     30
060900130325     C                   MOVE      WI15          W001A             1
061000130325     C     *IN30         IFEQ      *OFF
061100130325     C     W001A         ORLT      '0'
061200130325     C     W001A         ANDNE     *BLANK
061300130325     C                   SETON                                        32
061400130325     C                   MOVE      *ZERO         VABIAS
061500130325     C                   eval      vinmsg = %trimr(vinmsg)
061600130325     C                             + ' ' + 'VHBIAS'
061700130325     C                   ELSE
061800130325     C                   MOVEL     WI15          VABIAS
061900130325     C                   TESTN                   WD15                 30
062000130325     C  N30              TESTN                   WD15                   30
062100130325     C  N30              TESTN                   WD15                     30
062200130325     C                   MOVE      WD15          W001A             1
062300130325     C     *IN30         IFEQ      *OFF
062400130325     C     W001A         ORLT      '0'
062500130325     C     W001A         ANDNE     *BLANK
062600130325     C                   SETON                                        32
062700130325     C                   MOVE      *ZERO         VABIAS
062800130325     C                   eval      vinmsg = %trimr(vinmsg)
062900130325     C                             + ' ' + 'VHBIAS'
063000130325     C                   ELSE
063100130325     C                   MOVE      WD15          VABIAS
063200130325     C                   ENDIF
063300130325     C                   ENDIF
063400130325     C*
063500130325     C                   TESTN                   VHBNCL               30
063600130325     C  N30              TESTN                   VHBNCL                 30
063700130325     C  N30              TESTN                   VHBNCL                   30
063800130325     C                   MOVE      VHBNCL        W001A             1
063900130325     C     *IN30         IFEQ      *OFF
064000130325     C     W001A         ORLT      '0'
064100130325     C                   SETON                                        32
064200130325     C                   MOVE      *ZERO         VABNCL
064300130325     C                   eval      vinmsg = %trimr(vinmsg)
064400130325     C                             + ' ' + 'VHBNCL'
064500130325     C                   ELSE
064600130325     C                   MOVE      VHBNCL        VABNCL
064700130325     C                   ENDIF
064800130325     C*
064900130325     C                   MOVE      *ZERO         VABPKB
065000130325     C                   MOVE      VHBPKB        WASC09
065100130325     C                   IF        WI09 = *blanks
065200130325     C                   EVAL      WI09 = *zeros
065300130325     C                   ENDIF
065400130325     C                   TESTN                   WI09                 30
065500130325     C  N30              TESTN                   WI09                   30
065600130325     C  N30              TESTN                   WI09                     30
065700130325     C                   MOVE      WI09          W001A             1
065800130325     C     *IN30         IFEQ      *OFF
065900130325     C     W001A         ORLT      '0'
066000130325     C                   SETON                                        32
066100130325     C                   MOVE      *ZERO         VABPKB
066200130325     C                   eval      vinmsg = %trimr(vinmsg)
066300130325     C                             + ' ' + 'VHBPKB'
066400130325     C                   ELSE
066500130325     C                   MOVEL     WI09          VABPKB
066600130325     C                   TESTN                   WD09                 30
066700130325     C  N30              TESTN                   WD09                     30
066800130325     C                   MOVE      WD09          W001A             1
066900130325     C     *IN30         IFEQ      *OFF
067000130325     C     W001A         ORLT      '0'
067100130325     C     W001A         ANDNE     *BLANK
067200130325     C                   SETON                                        32
067300130325     C                   MOVE      *ZERO         VABPKB
067400130325     C                   eval      vinmsg = %trimr(vinmsg)
067500130325     C                             + ' ' + 'VHBPKB'
067600130325     C                   ELSE
067700130325     C                   MOVE      WD09          VABPKB
067800130325     C                   ENDIF
067900130325     C                   ENDIF
068000130325     C*
068100130325     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
068200130326     C                   IF        *IN51 = *OFF
068300130325     C                   MOVE      *ZERO         VABVLB
068400130325     C                   MOVE      VHBVLB        WASC07
068500130325     C                   IF        WI07 = *blanks
068600130325     C                   EVAL      WI07 = *zeros
068700130325     C                   ENDIF
068800130325     C                   TESTN                   WI07                 30
068900130325     C  N30              TESTN                   WI07                   30
069000130325     C  N30              TESTN                   WI07                     30
069100130325     C                   MOVE      WI07          W001A             1
069200130325     C     *IN30         IFEQ      *OFF
069300130325     C     W001A         ORLT      '0'
069400130325     C     W001A         ANDNE     *BLANK
069500130325     C                   SETON                                        32
069600130325     C                   MOVE      *ZERO         VABVLB
069700130325     C                   eval      vinmsg = %trimr(vinmsg)
069800130325     C                             + ' ' + 'VHBVLB'
069900130325     C                   ELSE
070000130325     C                   MOVEL     WI07          VABVLB
070100130325     C                   TESTN                   WD07                 30
070200130325     C  N30              TESTN                   WD07                   30
070300130325     C  N30              TESTN                   WD07                     30
070400130325     C                   MOVE      WD07          W001A             1
070500130325     C     *IN30         IFEQ      *OFF
070600130325     C     W001A         ORLT      '0'
070700130325     C     W001A         ANDNE     *BLANK
070800130325     C                   SETON                                        32
070900130325     C                   MOVE      *ZERO         VABVLB
071000130325     C                   eval      vinmsg = %trimr(vinmsg)
071100130325     C                             + ' ' + 'VHBVLB'
071200130325     C                   ELSE
071300130325     C                   MOVE      WD07          VABVLB
071400130325     C                   ENDIF
071500130325     C                   ENDIF
071600130326     C                   ELSE
071700130325     C* ...altrimenti forzo azzeramento del volume
071800130326     C                   Z-ADD     *zeros        VABVLB
071900130326     C                   ENDIF
072000130325     C*
072100130325     C                   MOVE      *ZERO         VABQFT
072200130325     C                   MOVE      VHBQFT        WASC15
072300130325     C                   TESTN                   WI15                 30
072400130325     C  N30              TESTN                   WI15                   30
072500130325     C  N30              TESTN                   WI15                     30
072600130325     C                   MOVE      WI15          W001A             1
072700130325     C     *IN30         IFEQ      *OFF
072800130325     C     W001A         ORLT      '0'
072900130325     C     W001A         ANDNE     *BLANK
073000130325     C                   SETON                                        32
073100130325     C                   MOVE      *ZERO         VABQFT
073200130325     C                   eval      vinmsg = %trimr(vinmsg)
073300130325     C                             + ' ' + 'VHBQFT'
073400130325     C                   ELSE
073500130325     C                   MOVEL     WI15          VABQFT
073600130325     C                   TESTN                   WD15                 30
073700130325     C  N30              TESTN                   WD15                   30
073800130325     C  N30              TESTN                   WD15                     30
073900130325     C                   MOVE      WD15          W001A             1
074000130325     C     *IN30         IFEQ      *OFF
074100130325     C     W001A         ORLT      '0'
074200130325     C     W001A         ANDNE     *BLANK
074300130325     C                   SETON                                        32
074400130325     C                   MOVE      *ZERO         VABQFT
074500130325     C                   eval      vinmsg = %trimr(vinmsg)
074600130325     C                             + ' ' + 'VHBQFT'
074700130325     C                   ELSE
074800130325     C                   MOVE      WD15          VABQFT
074900130325     C                   ENDIF
075000130325     C                   ENDIF
075100130325     C*
075200130325     C                   MOVE      *ZERO         VABCAS
075300130325     C                   MOVE      VHBCAS        WASC15
075400130325     C                   TESTN                   WI15                 30
075500130325     C  N30              TESTN                   WI15                   30
075600130325     C  N30              TESTN                   WI15                     30
075700130325     C                   MOVE      WI15          W001A             1
075800130325     C     *IN30         IFEQ      *OFF
075900130325     C     W001A         ORLT      '0'
076000130325     C     W001A         ANDNE     *BLANK
076100130325     C                   SETON                                        32
076200130325     C                   MOVE      *ZERO         VABCAS
076300130325     C                   eval      vinmsg = %trimr(vinmsg)
076400130325     C                             + ' ' + 'VHBCAS'
076500130325     C                   ELSE
076600130325     C                   MOVEL     WI15          VABCAS
076700130325     C                   TESTN                   WD15                 30
076800130325     C  N30              TESTN                   WD15                   30
076900130325     C  N30              TESTN                   WD15                     30
077000130325     C                   MOVE      WD15          W001A             1
077100130325     C     *IN30         IFEQ      *OFF
077200130325     C     W001A         ORLT      '0'
077300130325     C     W001A         ANDNE     *BLANK
077400130325     C                   SETON                                        32
077500130325     C                   MOVE      *ZERO         VABCAS
077600130325     C                   eval      vinmsg = %trimr(vinmsg)
077700130325     C                             + ' ' + 'VHBCAS'
077800130325     C                   ELSE
077900130325     C                   MOVE      WD15          VABCAS
078000130325     C                   ENDIF
078100130325     C                   ENDIF
078200130325     C*
078300130325     C                   TESTN                   VHBRMN               30
078400130325     C  N30              TESTN                   VHBRMN                 30
078500130325     C  N30              TESTN                   VHBRMN                   30
078600130325     C                   MOVE      VHBRMN        W001A             1
078700130325     C     *IN30         IFEQ      *OFF
078800130325     C     W001A         ORLT      '0'
078900130325     C                   SETON                                        32
079000130325     C                   MOVE      *ZERO         VABRMN
079100130325     C                   eval      vinmsg = %trimr(vinmsg)
079200130325     C                             + ' ' + 'VHBRMN'
079300130325     C                   ELSE
079400130325     C                   MOVE      VHBRMN        VABRMN
079500130325     C                   ENDIF
079600130325     C*
079700130325     C                   TESTN                   VHBNCD               30
079800130325     C  N30              TESTN                   VHBNCD                 30
079900130325     C  N30              TESTN                   VHBNCD                   30
080000130325     C                   MOVE      VHBNCD        W001A             1
080100130325     C     *IN30         IFEQ      *OFF
080200130325     C     W001A         ORLT      '0'
080300130325     C     W001A         ANDNE     *BLANK
080400130325     C                   SETON                                        32
080500130325     C                   MOVE      *ZERO         VABNCD
080600130325     C                   eval      vinmsg = %trimr(vinmsg)
080700130325     C                             + ' ' + 'VHBNCD'
080800130325     C                   ELSE
080900130325     C                   MOVE      VHBNCD        VABNCD
081000130325     C                   ENDIF
081100130325     C*
081200130325     C                   TESTN                   VHBNCA               30
081300130325     C  N30              TESTN                   VHBNCA                 30
081400130325     C  N30              TESTN                   VHBNCA                   30
081500130325     C                   MOVE      VHBNCA        W001A             1
081600130325     C     *IN30         IFEQ      *OFF
081700130325     C     W001A         ORLT      '0'
081800130325     C     W001A         ANDNE     *BLANK
081900130325     C                   SETON                                        32
082000130325     C                   MOVE      *ZERO         VABNCA
082100130325     C                   eval      vinmsg = %trimr(vinmsg)
082200130325     C                             + ' ' + 'VHBNCA'
082300130325     C                   ELSE
082400130325     C                   MOVE      VHBNCA        VABNCA
082500130325     C                   ENDIF
082600130325     C*
082700130325     C                   TESTN                   VHBZNC               30
082800130325     C  N30              TESTN                   VHBZNC                 30
082900130325     C  N30              TESTN                   VHBZNC                   30
083000130325     C                   MOVE      VHBZNC        W001A             1
083100130325     C     *IN30         IFEQ      *OFF
083200130325     C     W001A         ORLT      '0'
083300130325     C     W001A         ANDNE     *BLANK
083400130325     C                   SETON                                        32
083500130325     C                   MOVE      *ZERO         VABZNC
083600130325     C                   eval      vinmsg = %trimr(vinmsg)
083700130325     C                             + ' ' + 'VHBZNC'
083800130325     C                   ELSE
083900130325     C                   MOVE      VHBZNC        VABZNC
084000130325     C                   ENDIF
084100130325     C*
084200130325     C                   TESTN                   VHBDCR               30
084300130325     C  N30              TESTN                   VHBDCR                 30
084400130325     C  N30              TESTN                   VHBDCR                   30
084500130325     C                   MOVE      VHBDCR        W001A             1
084600130325     C     *IN30         IFEQ      *OFF
084700130325     C     W001A         ORLT      '0'
084800130325     C     W001A         ANDNE     *BLANK
084900130325     C                   SETON                                        32
085000130325     C                   MOVE      *ZERO         VABDCR
085100130325     C                   eval      vinmsg = %trimr(vinmsg)
085200130325     C                             + ' ' + 'VHBDCR'
085300130325     C                   ELSE
085400130325     C                   MOVE      VHBDCR        VABDCR
085500130325     C                   ENDIF
085600130325     C*
085700130325     C                   TESTN                   VHBHCR               30
085800130325     C  N30              TESTN                   VHBHCR                 30
085900130325     C  N30              TESTN                   VHBHCR                   30
086000130325     C                   MOVE      VHBHCR        W001A             1
086100130325     C     *IN30         IFEQ      *OFF
086200130325     C     W001A         ORLT      '0'
086300130325     C     W001A         ANDNE     *BLANK
086400130325     C                   SETON                                        32
086500130325     C                   MOVE      *ZERO         VABHCR
086600130325     C                   eval      vinmsg = %trimr(vinmsg)
086700130325     C                             + ' ' + 'VHBHCR'
086800130325     C                   ELSE
086900130325     C                   MOVE      VHBHCR        VABHCR
087000130325     C                   ENDIF
087100130325     C*
087200130325     C                   MOVE      *ZERO         VABVMD
087300130325     C                   MOVE      VHBVMD        WASC15
087400130325     C                   TESTN                   WI15                 30
087500130325     C  N30              TESTN                   WI15                   30
087600130325     C  N30              TESTN                   WI15                     30
087700130325     C                   MOVE      WI15          W001A             1
087800130325     C     *IN30         IFEQ      *OFF
087900130325     C     W001A         ORLT      '0'
088000130325     C     W001A         ANDNE     *BLANK
088100130325     C                   SETON                                        32
088200130325     C                   MOVE      *ZERO         VABVMD
088300130325     C                   eval      vinmsg = %trimr(vinmsg)
088400130325     C                             + ' ' + 'VHBVMD'
088500130325     C                   ELSE
088600130325     C                   MOVEL     WI15          VABVMD
088700130325     C                   TESTN                   WD15                 30
088800130325     C  N30              TESTN                   WD15                   30
088900130325     C  N30              TESTN                   WD15                     30
089000130325     C                   MOVE      WD15          W001A             1
089100130325     C     *IN30         IFEQ      *OFF
089200130325     C     W001A         ORLT      '0'
089300130325     C     W001A         ANDNE     *BLANK
089400130325     C                   SETON                                        32
089500130325     C                   MOVE      *ZERO         VABVMD
089600130325     C                   eval      vinmsg = %trimr(vinmsg)
089700130325     C                             + ' ' + 'VHBVMD'
089800130325     C                   ELSE
089900130325     C                   MOVE      WD15          VABVMD
090000130325     C                   ENDIF
090100130325     C                   ENDIF
090200130325     C*
090300130325     C                   TESTN                   VHBANT               30
090400130325     C  N30              TESTN                   VHBANT                 30
090500130325     C  N30              TESTN                   VHBANT                   30
090600130325     C                   MOVE      VHBANT        W001A             1
090700130325     C     *IN30         IFEQ      *OFF
090800130325     C     W001A         ORLT      '0'
090900130325     C     W001A         ANDNE     *BLANK
091000130325     C                   SETON                                        32
091100130325     C                   MOVE      *ZERO         VABANT
091200130325     C                   eval      vinmsg = %trimr(vinmsg)
091300130325     C                             + ' ' + 'VHBANT'
091400130325     C                   ELSE
091500130325     C                   MOVE      VHBANT        VABANT
091600130325     C                   ENDIF
091700130325     C*
091800130325     C* CAMPI ALFABETICI:
091900130325     C                   MOVE      VHBATB        VABATB
092000130418     C* il fòag di annullamento deve essere sempre blank
092100130418     C                   CLEAR                   VABATB
092200130325     C                   MOVE      VHBCBO        VABCBO
092300130325     C                   MOVE      VHBRSD        VABRSD
092400130325     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092500130325     C     '@':'A'       XLATE     VABRSD        VABRSD
092600130325     C* ==
092700130325     C                   MOVE      VHBRD2        VABRD2
092800130325     C                   MOVE      VHBIND        VABIND
092900130325     C                   MOVE      VHBCAD        VABCAD
093000130325     C                   MOVE      VHBLOD        VABLOD
093100130325     C                   MOVE      VHBPRD        VABPRD
093200130325     C                   MOVE      VHBNZD        VABNZD
093300130325     C                   MOVE      VHBGC1        VABGC1
093400130325     C                   MOVE      VHBGC2        VABGC2
093500130325     C                   MOVE      VHBTSP        VABTSP
093600130325     C                   MOVE      VHBVAS        VABVAS
093700130325     C                   MOVE      VHBNAS        VABNAS
093800130325     C                   MOVE      VHBTIC        VABTIC
093900130325     C                   MOVE      VHBVCA        VABVCA
094000130325     C                   MOVE      VHBGCA        VABGCA
094100130325     C                   MOVE      VHBRMA        VABRMA
094200130325     C                   MOVE      VHBXCO        VABXCO
094300130325     C                   MOVE      VHBNOT        VABNOT
094400130325     C                   MOVE      VHBNT2        VABNT2
094500130325     C                   MOVE      VHBCTM        VABCTM
094600130325     C* VABCTM è forzato 7Q
094700130325     C                   MOVEL     '7Q'          VABCTM
094800130325     C                   MOVE      VHBFFD        VABFFD
094900130325     C                   MOVE      VHBTCR        VABTCR
095000130325     C                   MOVE      VHBCTS        VABCTS
095100130325     C                   MOVE      VHBFTM        VABFTM
095200130325     C                   MOVE      VHBVAD        VABVAD
095300130325     C                   MOVE      VHBGMA        VABGMA
095400130325     C                   MOVE      VHBGGA        VABGGA
095500130325     C                   MOVE      VHBGVA        VABGVA
095600130325     C                   MOVE      VHBTC1        VABTC1
095700130325     C                   MOVE      VHBTC2        VABTC2
095800130325     C                   MOVE      VHBSCL        VABSCL
095900130325     C                   MOVE      VHBRMO        VABRMO
096000130325     C                   MOVE      VHBCMO        VABCMO
096100130325     C                   MOVE      VHBNMO        VABNMO
096200130325     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
096300130325     C* NELLA NAZIONE MITTENTE ORIGINALE
096400130325     C                   IF        %trim(VHBNMO) < 'A'
096500130325     C                   MOVE      *BLANKS       VABNMO
096600130325     C                   ENDIF
096700130325     C*
096800130325     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
096900130325     C                   IF        %trim(VABNZD) = 'I'   OR
097000130325     C                             %trim(VABNZD) = 'IT'  OR
097100130325     C                             %trim(VABNZD) = 'ITA'
097200130325     C                   EVAL      VABNZD = *blanks
097300130325     C                   ENDIF
097400130325     C*
097500130325     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
097600130326     C                   IF        *IN52 = *ON
097700130325     C* Reperisco la provincia dal CAP e dalla località
097800130325     C                   IF        VABCAD <> *blanks AND
097900130325     C                             VABPRD  = *blanks AND
098000130325     C                             VABNZD  = *blanks
098100130325     C                   CLEAR                   TISI95DS
098200130325     C                   EVAL      I95TCN = '3'
098300130325     C                   Z-ADD     datcor        I95DAT
098400130325     C                   EVAL      I95CAP = VABCAD
098500130325     C                   EVAL      I95LOC = VABLOD
098600130325     C                   CALL      'TISI95R'
098700130325     C                   PARM                    TISI95DS
098800130325     C                   EVAL      VABPRD = O95PRV
098900130325     C                   ENDIF
099000130326     C                   ENDIF
099100130325     C*
099200130325     C* "bonifico" i dati d input da eventuali caratteri indesiderati
099300130325     C                   exsr      exeBon
099400130325     C*
099500130325     C* Forzatura su campi DIVISE
099600130325     C                   IF        VABVAS = *blanks
099700130325     C                   EVAL      VABVAS = 'EUR'
099800130325     C                   ENDIF
099900130325     C                   IF        VABVCA = *blanks
100000130325     C                   EVAL      VABVCA = 'EUR'
100100130325     C                   ENDIF
100200130325     C                   IF        VABVAD = *blanks
100300130325     C                   EVAL      VABVAD = 'EUR'
100400130325     C                   ENDIF
100500130325     C*
100600130325     C                   ENDIF
100700130325     C*
100800130325     C                   ENDSR
100900130325     C***
101000130325     C*----------------------------------------------------*
101100130325
101200130325     C*----------------------------------------------------*
101300130325     C     VATStdEBCDIC  BEGSR
101400130325     C*
101500130325     C                   SETOFF                                       3132
101600130325     C*
101700130325     C* Reperimento campi ALFA
101800130325     C                   EVAL      VATATB=a_vatatb
101900130418     C* il fòag di annullamento deve essere sempre blank
102000130418     C                   CLEAR                   VATATB
102100130325     C                   EVAL      VATTRC=a_vattrc
102200130325     C                   EVAL      VATNOT=%trim(a_vatnot)
102300130325     C*
102400130325     C                   EVAL      PiStr=%trim(a_vatccm)
102500130325     C                   EXSR      CHKNUM
102600130325     C                   IF        PiInt=*on
102700130325     C                   Z-ADD     PiVal         VATCCM
102800130325     C                   ELSE
102900130325     C                   SETON                                        31
103000130325     C                   EVAL      vinmsg = %trimr(vinmsg)
103100130325     C                             + ' ' + 'VATCCM'
103200130325     C                   ENDIF
103300130325     C*
103400130325     C                   EVAL      PiStr=%trim(a_vatlnp)
103500130325     C                   EXSR      CHKNUM
103600130325     C                   IF        PiInt=*on
103700130325     C                   Z-ADD     PiVal         VATLNP
103800130325     C                   ELSE
103900130325     C                   SETON                                        31
104000130325     C                   EVAL      vinmsg = %trimr(vinmsg)
104100130325     C                             + ' ' + 'VATLNP'
104200130325     C                   ENDIF
104300130325     C*
104400130325     C                   EVAL      PiStr=%trim(a_vataas)
104500130325     C                   EXSR      CHKNUM
104600130325     C                   IF        PiInt=*on
104700130325     C                   Z-ADD     PiVal         VATAAS
104800130325     C                   ELSE
104900130325     C                   SETON                                        31
105000130325     C                   EVAL      vinmsg = %trimr(vinmsg)
105100130325     C                             + ' ' + 'VATAAS'
105200130325     C                   ENDIF
105300130325     C*
105400130325     C                   EVAL      PiStr=%trim(a_vatnrs)
105500130325     C                   IF        PiStr = *blanks
105600130325     C                   EVAL      PiStr = *zeros
105700130325     C                   ELSE
105800130325     C                   EXSR      CHKNUM
105900130325     C                   IF        PiInt=*on
106000130325     C                   Z-ADD     PiVal         VATNRS
106100130325     C                   ELSE
106200130325     C                   SETON                                        31
106300130325     C                   EVAL      vinmsg = %trimr(vinmsg)
106400130325     C                             + ' ' + 'VATNRS'
106500130325     C                   ENDIF
106600130325     C                   ENDIF
106700130326     C***
106800130326     C* siccome reperisco il nr.spedizione da trul33, non devo prenderlo da file
106900130325     C***                EVAL      PiStr=%trim(a_vatnsp)
107000130325     C***                EXSR      CHKNUM
107100130325     C***                IF        PiInt=*on
107200130325     C***                Z-ADD     PiVal         VATNSP
107300130325     C***                ELSE
107400130325     C***                SETON                                        31
107500130325     C***                EVAL      vinmsg = %trimr(vinmsg)
107600130325     C***                          + ' ' + 'VATNSP'
107700130325     C***                ENDIF
107800130325     C*
107900130325     C                   ENDSR
108000130325     C*----------------------------------------------------*
108100010202     C*----------------------------------------------------*
108200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
108300010202     C*----------------------------------------------------*
108400020305     C     PREVAT        BEGSR
108500010202     C*
108600021113     C* Compongo il nome del membro da dare al FIVATWWR
108700010202     C                   eval      parmbr = vlrhdl
108800010202     C                   movel     'M'           parmbr
108900060113     C                   eval      parccm = vlrksc
109000010202     C                   eval      paropz = '1'
109100010202     C* Effettuo la chiamata al CLLE preposto
109200040506     C                   call(e)   'TITVVTC'
109300010202     C                   parm                    parccm
109400010202     C                   parm                    parmbr
109500010202     C                   parm                    paropz
109600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
109700010202     C                   if        %error
109800010202     C                   movel     '1'           chkcall
109900010202     C                   else
110000010202     C                   movel     '0'           chkcall
110100010202     C                   endif
110200010202     C*
110300010202     C                   ENDSR
110400000801     C*----------------------------------------------------*
110500000801     C*  CONTROLLO NUMERICITA' CAMPI
110600000801     C*----------------------------------------------------*
110700000801     C     CHKNUM        BEGSR
110800000801     C*
110900000801     C                   call(e)   'ISNUMERIC'
111000000801     C                   PARM                    PiStr            30
111100040714     C                   PARM      ','           PiDecChr          1
111200000801     C                   PARM      *ZEROS        PiVal            30 9
111300000801     C                   PARM      '0'           PiInt             1
111400000801     C                   PARM      '0'           PiNum             1
111500000801     C                   IF        %error
111600000801     C                   EVAL      PiInt=*off
111700000801     C                   ENDIF
111800000801     C*
111900000801     C                   ENDSR
112000000801     C***
112100000801
112200011113
112300011113     C*----------------------------------------------------*
112400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
112500011113     C*----------------------------------------------------*
112600011113     C     CHKIMPDIV     BEGSR
112700011113     C*
112800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
112900011113     C                   Z-ADD     *zeros        wrkDec            9 9
113000011113     C*
113100011113     C* Come prima cosa effettuo considerazioni sulla divisa
113200011113     C                   IF        vabIAS > *zeros
113300011113     C                   IF        vabVAS <> 'EUR'
113400011113     C                   EVAL      vabVAS =  'ITL'
113500011113     C                   ENDIF
113600011113     C                   ENDIF
113700011113     C*
113800011113     C                   IF        vabCAS > *zeros
113900011113     C                   IF        vabVCA <> 'EUR'
114000011113     C                   EVAL      vabVCA =  'ITL'
114100011113     C                   ENDIF
114200011113     C                   ENDIF
114300011113     C*
114400011113     C                   IF        vabVMD > *zeros
114500020305     C                   IF        vabVAD <> 'EUR'
114600011113     C                   EVAL      vabVAD =  'ITL'
114700011113     C                   ENDIF
114800011113     C                   ENDIF
114900011113     C*
115000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
115100011113     C                   Z-ADD     vabIAS        wrkDec
115200011113     C                   IF        wrkDec > *zeros
115300011113     C                   IF        vabVAS = 'ITL'
115400011113     C                   EVAL      vabIAS = *zeros
115500011113     C                   ENDIF
115600011113     C                   ENDIF
115700011113     C*
115800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
115900011113     C                   Z-ADD     vabCAS        wrkDec
116000011113     C                   IF        wrkDec > *zeros
116100011113     C                   IF        vabVCA = 'ITL'
116200011113     C                   EVAL      vabCAS = *zeros
116300011113     C                   ENDIF
116400011113     C                   ENDIF
116500011113     C*
116600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
116700011113     C                   Z-ADD     vabVMD        wrkDec
116800011113     C                   IF        wrkDec > *zeros
116900011113     C                   IF        vabVAD = 'ITL'
117000011113     C                   EVAL      vabVMD = *zeros
117100011113     C                   ENDIF
117200011113     C                   ENDIF
117300011113     C*
117400011113     C                   ENDSR
117500011113     C***
117600011113
117700011113
117800000801
117900000801
118000990920      /TITLE Invio dei dati al punto operativo.
118100010202     C     invio         BEGSR
118200990920     C*
118300021113     C* 1° invio FIVAT
118400010201     C                   reset                   dscmz
118500010201     C                   move      vlrpoi        cmzdst
118600021113     C                   eval      cmzfld = 'FIVATWWR'
118700010201     C                   eval      cmzmbd = vlrhdl
118800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
118900021009     C***                if        prmfir = *blanks
119000021113     C                   eval      cmzfla = 'FIVAT00F'
119100021113     C                   eval      cmzmba = 'FIVAT00F'
119200021009     C***                else
119300021009     C***                eval      cmzfla = prmfir
119400021009     C***                eval      cmzmba = prmfir
119500021009     C***                endif
119600010201     C                   eval      cmznrr = *zeros
119700020305     C                   move      §ctrokvt      cmznrr
119800021018     C                   eval      cmzlba = vlrfl1
119900010201     C                   call(e)   'TIS711C'
120000010201     C                   parm                    dscmz
120100010201     C                   parm      *blanks       esito
120200010205     C                   if        %error
120300010205     C                             or cmzerr = '1'
120400010205     C                             or esito  = '1'
120500010205     C                   eval      wrkesito = '3'
120600010205     C                   else
120700010201     C*
120800021113     C* 2° invio FIVAB
120900010201     C                   reset                   dscmz
121000010201     C                   move      vlrpoi        cmzdst
121100010201     C                   eval      cmzfld = vlrfou
121200010201     C                   eval      cmzmbd = vlrhdl
121300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
121400021009     C***                if        prmfir = *blanks
121500021113     C                   eval      cmzfla = 'FIVAB00F'
121600021113     C                   eval      cmzmba = 'FIVAB00F'
121700021009     C***                else
121800021009     C***                eval      cmzfla = prmfir
121900021009     C***                eval      cmzmba = prmfir
122000021009     C***                endif
122100010201     C                   eval      cmznrr = *zeros
122200010201     C                   move      §ctrokvb      cmznrr
122300021018     C                   eval      cmzlba = vlrfl1
122400010201     C                   call(e)   'TIS711C'
122500010201     C                   parm                    dscmz
122600010201     C                   parm      *blanks       esito
122700010201     C                   if        %error
122800010201     C                             or cmzerr = '1'
122900010201     C                             or esito  = '1'
123000010201     C                   eval      wrkesito = '3'
123100010201     C                   endif
123200010205     C                   endif
123300990920     C*
123400000613     C                   ENDSR
123500000613     C***
123600070411
123700070411     C     *pssr         BEGSR
123800070411     C*
123900070411     C                   if        %open(tivin00r)
124000070411     C                   close     tivin00r
124100070411     C                   endif
124200070411     C                   if        %open(fivabwwr)
124300070411     C                   close     fivabwwr
124400070411     C                   endif
124500070411     C                   if        %open(fivatwwr)
124600070411     C                   close     fivatwwr
124700070411     C                   endif
124800070411     C*
124900070411     C* Effettuo la chiamata al CLLE preposto
125000070411     C                   call(e)   'TITVVTC'
125100070411     C                   parm                    parccm
125200070411     C                   parm                    parmbr
125300070411     C                   parm      '2'           paropz
125400070411     C*
125500070411     C                   eval      wrkesito = '2'
125600070411     C*
125700070411     C                   seton                                        LR
125800070411     C*
125900070411     C                   ENDSR     '*CANCL'
126000070411     C***
126100130325
126200130325      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
126300130325     C     exeBon        BEGSR
126400130325     C*
126500130325     C* Imposto i parametri x il *pgm d "bonifica"
126600130325     C                   EVAL      ElencoChar = *blanks
126700130325     C                   EVAL      TipoElenco = *blanks
126800130325     C                   EVAL      CharSost   = *blanks
126900130325     C                   EVAL      UpperCase  = '1'
127000130325     C                   EVAL      ChkNull    = '1'
127100130325     C                   EVAL      CharNull   = *blanks
127200130325     C*
127300130325     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
127400130325     C                   EVAL      TxtInOut   = VABRSD
127500130325     C                   EVAL      EsitoBon   = *blanks
127600130325     C*
127700130325     C                   CALL      'XCHKCHAR'
127800130325     C                   PARM                    TxtInOut
127900130325     C                   PARM                    ElencoChar
128000130325     C                   PARM                    TipoElenco
128100130325     C                   PARM                    CharSost
128200130325     C                   PARM                    UpperCase
128300130325     C                   PARM                    ChkNull
128400130325     C                   PARM                    CharNull
128500130325     C                   PARM                    EsitoBon
128600130325     C*
128700130325     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
128800130325     C                   IF        EsitoBon = '1'
128900130325     C                   EVAL      VABRSD = TxtInOut
129000130325     C                   ENDIF
129100130325     C*
129200130325     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
129300130325     C                   EVAL      TxtInOut   = VABIND
129400130325     C                   EVAL      EsitoBon   = *blanks
129500130325     C*
129600130325     C                   CALL      'XCHKCHAR'
129700130325     C                   PARM                    TxtInOut
129800130325     C                   PARM                    ElencoChar
129900130325     C                   PARM                    TipoElenco
130000130325     C                   PARM                    CharSost
130100130325     C                   PARM                    UpperCase
130200130325     C                   PARM                    ChkNull
130300130325     C                   PARM                    CharNull
130400130325     C                   PARM                    EsitoBon
130500130325     C*
130600130325     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
130700130325     C                   IF        EsitoBon = '1'
130800130325     C                   EVAL      VABIND = TxtInOut
130900130325     C                   ENDIF
131000130325     C*
131100130325     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
131200130325     C                   EVAL      TxtInOut   = VABLOD
131300130325     C                   EVAL      EsitoBon   = *blanks
131400130325     C*
131500130325     C                   CALL      'XCHKCHAR'
131600130325     C                   PARM                    TxtInOut
131700130325     C                   PARM                    ElencoChar
131800130325     C                   PARM                    TipoElenco
131900130325     C                   PARM                    CharSost
132000130325     C                   PARM                    UpperCase
132100130325     C                   PARM                    ChkNull
132200130325     C                   PARM                    CharNull
132300130325     C                   PARM                    EsitoBon
132400130325     C*
132500130325     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
132600130325     C                   IF        EsitoBon = '1'
132700130325     C                   EVAL      VABLOD = TxtInOut
132800130325     C                   ENDIF
132900130325     C*
133000130325     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
133100130325     C                   EVAL      TxtInOut   = VABNOT
133200130325     C                   EVAL      EsitoBon   = *blanks
133300130325     C                   EVAL      UpperCase  = *blanks
133400130325     C*
133500130325     C                   CALL      'XCHKCHAR'
133600130325     C                   PARM                    TxtInOut
133700130325     C                   PARM                    ElencoChar
133800130325     C                   PARM                    TipoElenco
133900130325     C                   PARM                    CharSost
134000130325     C                   PARM                    UpperCase
134100130325     C                   PARM                    ChkNull
134200130325     C                   PARM                    CharNull
134300130325     C                   PARM                    EsitoBon
134400130325     C*
134500130325     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
134600130325     C                   IF        EsitoBon = '1'
134700130325     C                   EVAL      VABNOT = TxtInOut
134800130325     C                   ENDIF
134900130325     C*
135000130325     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
135100130325     C                   EVAL      TxtInOut   = VABNT2
135200130325     C                   EVAL      EsitoBon   = *blanks
135300130325     C                   EVAL      UpperCase  = *blanks
135400130325     C*
135500130325     C                   CALL      'XCHKCHAR'
135600130325     C                   PARM                    TxtInOut
135700130325     C                   PARM                    ElencoChar
135800130325     C                   PARM                    TipoElenco
135900130325     C                   PARM                    CharSost
136000130325     C                   PARM                    UpperCase
136100130325     C                   PARM                    ChkNull
136200130325     C                   PARM                    CharNull
136300130325     C                   PARM                    EsitoBon
136400130325     C*
136500130325     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
136600130325     C                   IF        EsitoBon = '1'
136700130325     C                   EVAL      VABNT2 = TxtInOut
136800130325     C                   ENDIF
136900130325     C*
137000130325     C                   ENDSR
137100070411
137200990910
137300000613     C     *inzsr        BEGSR
137400990910     C*
137500990910     C     *entry        plist
137600990920     C                   parm                    tivlrds
137700990921     C                   parm      wrkesito      esito
137800000724     C                   parm                    prmlit
137900000710     C                   parm                    prmfir
138000000613     C*
138100000830     C* CALCOLA LA DATA CORRENTE
138200100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
138300100722     C                   eval      datcor = %dec(%date() : *ISO)
138400000830     C*
138500000613     C                   ENDSR
138600000613     C***
