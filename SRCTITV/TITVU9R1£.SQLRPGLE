000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500100412     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070730     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100120524     D depspe          s              9    INZ(*blanks)
002200120524     D curspe          s              9    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900120327     D* mi serve staccare numeratore per nr.spedizione?
003000120327     D* lo si deve capire dai dati passati
003100120327     D* (se il loro nr.spedizione è > 7 char, bisogna rinumerarla)
003200120327     D* IN QUESTO CASO NO
003300120327     D StaccoNrSped    s               n   inz(*off)
003400020725
003500020725     D*------------------
003600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003700020725     D*------------------
003800120327     D INPUT_DS        DS                  INZ
003900100729     D  VINDTA                     2048
004000100729     D  VINFLG                        1
004100120524     D  VINSPE                        9
004200100729     D  VINRRN                        8  0
004300020725     D*
004400081113
004500091223     D*------------------
004600091223     D* DS REPERIMENTO NUMERATORE
004700091223     D*------------------
004800100309     D trul33ds      e ds                  inz
004900091223     D*------------------
005000091223     D* DS ARCHITETTURA
005100091223     D*------------------
005200091223     D kpjba         e ds                  inz
005300091223
005400101119
005500101119     D*-------------------
005600101119     D* COSTANTI
005700101119     D*-------------------
005800101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005900101119     D                                     èéù')
006000101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006100101119     D                                     EEU')
006200081113
006300081113     D*------------------
006400081113     D* LINKING A DEFINIZIONI ESTERNE
006500081113     D*------------------
006600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006800081113
006900990908
007000010201
007100010201
007200080117     C*
007300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007400080117     C
007500080117     C/EXEC SQL
007600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007700080117     C/END-EXEC
007800080117     C
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200000613     C*
008300100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008500000613     C*
008600010202     C* Effettuo la chiamata al CLLE preposto
008700100525     C  N51              call(e)   'TITVVTC'
008800100525     C                   parm                    parccm
008900100525     C                   parm                    parmbr
009000100525     C                   parm      '2'           paropz
009100100525     C   51              call(e)   'TITVVBC'
009200100525     C                   parm                    parccm
009300100525     C                   parm                    parmbr
009400100525     C                   parm      '2'           paropz
009500070730     C*
009600070730     C* Effettuo lancio TISI95 solo x chiusura
009700070730     C                   CLEAR                   TISI95DS
009800070730     C                   EVAL      I95TLA = 'C'
009900070730     C                   CALL      'TISI95R'
010000070730     C                   PARM                    TISI95DS
010100000616     C*
010200000801     C
010300010201     C                   seton                                        LR
010400990908
010500000801
010600910830     C*--------------------------------------------------------
010700070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010800910830     C*--------------------------------------------------------
010900070530     C     RWFILE        BEGSR
011000990910     C*
011100990914     C                   if        not %open(tivin00r)
011200990908     C                   open      tivin00r
011300990914     C                   endif
011400070530     C*
011500100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011600100525     C                   exsr      prevasx
011700010201     C*
011800010202     C                   if        chkcall = '0'
011900010202     C*
012000100525     C                   if        not %open(fivabwwr)
012100100525     C                   open      fivabwwr
012200100525     C                   endif
012300100525     C*
012400021113     C                   if        not %open(fivatwwr)
012500021113     C                   open      fivatwwr
012600010201     C                   endif
012700080117     C*
012800080117     C                   EXSR      INZVAR
012900080117     C                   EXSR      DEFCAM
013000990910     C*
013100010201     C                   clear                   §CTROKVB          5 0
013200020305     C                   clear                   §CTROKVT          5 0
013300000801     C                   clear                   §CTRMO            5 0
013400000801     C                   clear                   §CTRNO            5 0
013500990910     C*
013600020725     C/EXEC SQL
013700020725     C+ declare C1 cursor for select
013800120327     C+ vindta, vinflg, substr(vindta, 12, 7) as sped, rrn(tivin00r)
013900060223     C+ from tivin00r
014000111020     C+ order by sped
014100020725     C+ for read only
014200020725     C/END-EXEC
014300020725     C
014400020725     C/EXEC SQL
014500020725     C+ open C1
014600020725     C/END-EXEC
014700020725     C
014800020725     C/EXEC SQL
014900070530     C+ Fetch C1 into :INPUT_DS
015000020725     C/END-EXEC
015100020725     C*
015200020725     C                   dow       sqlcod = *zeros
015300990913     C*
015400100310     C                   if        vindta > *blanks
015500000613     C                   add       1             rrnum
015600000801     C*
015700020725     C                   if        vinflg = *blanks
015800020725     C                             or vinflg = '0'
015900020725     C                             or vinflg = '2'
016000000801     C*
016100020725     C                   clear                   x_vinmsg
016200020725     C                   eval      x_vinflg = '1'
016300101119     C*
016400101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016500101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016600010305     C*
016700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016800120524     C                   EVAL      curspe=%trim(%subst(vindta:187:9))
016900010305     C*
017000100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017100080923     C                   if        *in50 = *off
017200101119     C                   exsr      inzvar
017300101119     C                   exsr      defcam
017400071003     C                   exsr      impvab
017500100525     C                   exsr      wrivab
017600100127     C                   exsr      wrivat_a                                     => carico VAT
017700071003     C                   exsr      wrivat_b                                     => carico VAT
017800100525     C   51              exsr      wrivat_e                                     => carico VAT
017900120524     C                   exsr      wrivat_i                                     => carico VAT
018000071003     C                   else
018100080923     C*
018200071009     C                   if        wDISK = 'D'
018300091223     C                   exsr      repNSP
018400071009     C                   exsr      impvab
018500100412     C                   exsr      wrivab
018600100127     C                   exsr      wrivat_a                                     => carico VAT
018700071009     C                   exsr      wrivat_b                                     => carico VAT
018800071009     C                   exsr      wrivat_e                                     => carico VAT
018900120524     C                   exsr      wrivat_i                                     => carico VAT
019000071009     C                   else
019100071009     C*
019200010305     C                   if        depspe = *blanks                             => 1° giro
019300010305     C                   eval      depspe = curspe                              => memorizz. spediz
019400080117     C                   clear                   tivinds
019500091223     C                   exsr      repNSP
019600020305     C                   exsr      impvab
019700100127     C                   exsr      wrivat_a                                     => carico VAT
019800071003     C                   exsr      wrivat_b                                     => carico VAT
019900071003     C   50              exsr      wrivat_e                                     => carico VAT
020000120524     C                   exsr      wrivat_i                                     => carico VAT
020100010305     C                   else
020200020725     C                   if        depspe <> curspe                             => rottura di spediz
020300010305     C                   eval      depspe = curspe                              => memorizz. spediz
020400100412     C                   exsr      wrivab
020500080117     C                   clear                   tivinds
020600091223     C                   exsr      repNSP
020700020305     C                   exsr      impvab
020800100127     C                   exsr      wrivat_a                                     => carico VAT
020900071003     C                   exsr      wrivat_b                                     => carico VAT
021000071003     C   50              exsr      wrivat_e                                     => carico VAT
021100120524     C                   exsr      wrivat_i                                     => carico VAT
021200020305     C                   else                                                   => x stessa spediz
021300100401     C*
021400100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021500100928     C                   select
021600100928     C                   when      wDUPREC = ' '
021700020305     C                   exsr      impvab
021800100127     C                   exsr      wrivat_a                                     => carico VAT
021900071003     C                   exsr      wrivat_b                                     => carico VAT
022000071003     C   50              exsr      wrivat_e                                     => carico VAT
022100120524     C                   exsr      wrivat_i                                     => carico VAT
022200100928     C                   when      wDUPREC = 'C'
022300100928     C   50              exsr      wrivat_e                                     => carico VAT
022400100928     C                   endsl
022500010305     C                   endif
022600010305     C                   endif
022700010305     C                   endif
022800071003     C                   endif
022900110201     C*
023000071009     C                   endif
023100000905     C*
023200000905     C                   else
023300020725     C                   eval      x_vinflg = '1'
023400000905     C                   endif
023500000905     C*
023600020725     C     VINRRN        chain     tivin000
023700020725     C                   if        %found(tivin00r)
023800020725     C                   eval      y_vinflg = x_vinflg
023900020725     C                   eval      y_vinmsg = x_vinmsg
024000020725     C                   update    tivin000
024100020725     C                   endif
024200020725     C*
024300020725     C/EXEC SQL
024400070530     C+ Fetch C1 into :INPUT_DS
024500020725     C/END-EXEC
024600020725     C*
024700020725     C                   enddo
024800020725     C*
024900020725     C/EXEC SQL
025000020725     C+ close C1
025100020725     C/END-EXEC
025200000905     C*
025300020305     C* Scarico i VAB rimasti "in sospeso"
025400071009     C                   if        wDISK = 'C'
025500100412     C                   exsr      wrivab
025600071009     C                   endif
025700010202     C*
025800010202     C                   endif
025900990910
026000990910     C* Se non ci sono record con errori ...
026100000710     C                   if        §ctrno = 0
026200990910     C* ... restituisco esito OK.
026300990921     C                   eval      wrkesito = '0'
026400990910     C                   else
026500100525     C                   if        §ctrokvb > 0 OR
026600100525     C                             §ctrokvt > 0
026700990921     C                   eval      wrkesito = '1'
026800000710     C                   else
026900000710     C                   eval      wrkesito = '2'
027000990910     C                   endif
027100000710     C                   endif
027200990910     C*
027300990914     C                   if        %open(tivin00r)
027400990908     C                   close     tivin00r
027500990914     C                   endif
027600021113     C                   if        %open(fivabwwr)
027700021113     C                   close     fivabwwr
027800990914     C                   endif
027900021113     C                   if        %open(fivatwwr)
028000021113     C                   close     fivatwwr
028100010201     C                   endif
028200990910     C*
028300100525     C                   if        §ctrokvb > 0 OR
028400100525     C                             §ctrokvt > 0
028500000724     C                             and vlrpoi <> *zeros
028600010202     C                   exsr      invio
028700990920     C                   endif
028800990920     C*
028900910830     C                   ENDSR
029000000613     C***
029100100412
029200100412
029300010305
029400010305     C*----------------------------------------------------*
029500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029600010305     C*----------------------------------------------------*
029700020305     C     WRIVAB        BEGSR
029800100412     C*
029900100412     C* Gestisco l'eventuale rottura x numero spedizione
030000100412     C                   if        VABRMA = *blanks
030100100412     C                   movel(P)  VABRMN        VABRMA
030200100412     C                   endif
030300100412     C*
030400100525     C                   if        *in51 = *off and *in31 = *off
030500100525     C                   write     fivab000                                     => scarico il VAB
030600100525     C                   endif
030700010305     C*
030800010305     C                   ENDSR
030900990920
031000000801     C*----------------------------------------------------*
031100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031200000801     C*----------------------------------------------------*
031300010201     C     INZVAR        BEGSR
031400000801     C*
031500010201     C                   Z-ADD     *zeros        Num5_0
031600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031700020725     C                   MOVEL     '0'           FlgCAS            1
031800000801     C*
031900000801     C                   ENDSR
032000000801     C*----------------------------------------------------*
032100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032200000801     C*----------------------------------------------------*
032300020904     C     DEFCAM        BEGSR
032400080923     C*
032500100928     C                   SETOFF                                       505160
032600000801     C*
032700021113     C                   CLEAR                   FIVAB000
032800021113     C                   CLEAR                   FIVAT000
032900070730     C* Imposto i valori di default...
033000120529     C                   EVAL      VABCCM = 2021491
033100120529     C                   EVAL      VATCCM = 2021491
033200120529     C                   EVAL      VABLNP = 202
033300120529     C                   EVAL      VATLNP = 202
033400111110     C                   EVAL      VABCTR = 000
033500120529     C                   EVAL      VABTSP = 'C'
033600100525     C                   EVAL      VABCBO = '1'
033700070222     C* ... e poi verifico se sono stati passati come parametri
033800070222     C                   IF        vlrppt > *blanks
033900100525     C*
034000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034200070222     C                   EXSR      CHKNUM
034300070222     C                   IF        PiInt=*on
034400070222     C                   Z-ADD     PiVal         VABCCM
034500070222     C                   Z-ADD     PiVal         VATCCM
034600070222     C                   ENDIF
034700070222     C                   ENDIF
034800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035000070222     C                   EXSR      CHKNUM
035100070222     C                   IF        PiInt=*on
035200070222     C                   Z-ADD     PiVal         VABLNP
035300070222     C                   Z-ADD     PiVal         VATLNP
035400070222     C                   ENDIF
035500070222     C                   ENDIF
035600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035800070222     C                   EXSR      CHKNUM
035900070222     C                   IF        PiInt=*on
036000070222     C                   Z-ADD     PiVal         VABCTR
036100070222     C                   ENDIF
036200070928     C                   ENDIF
036300071009     C                   MOVEL     *blanks       wDISK             1
036400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036500071009     C                   IF        wDISK <> *blanks
036600070928     C                   SETON                                        50
036700070222     C                   ENDIF
036800100525     C                   IF        wDISK  = 'T'
036900100525     C                   SETOFF                                       50
037000100525     C                   SETON                                        51
037100100525     C                   ENDIF
037200100401     C                   MOVEL     *blanks       wDUPREC           1
037300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037400100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037500100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037600100525     C                   ENDIF
037700100525     C*
037800070222     C                   ENDIF
037900071009     C*
038000071009     C   50              EVAL      VABCTM = '7Q'
038100000801     C*
038200000801     C                   ENDSR
038300091223     C*----------------------------------------------------*
038400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038500091223     C*----------------------------------------------------*
038600091223     C     REPNSP        BEGSR
038700091223     C*
038800120327     C                   IF        StaccoNrSped = *on
038900091223     C                   EXSR      INZVAR
039000091223     C                   EXSR      DEFCAM
039100100928     C*
039200100928     C                   SETON                                        60
039300091223     C*
039400091223     C* NSP => Stacco un numeratore da AZNUM
039500091223     C                   clear                   TRUL33DS
039600091223     C                   eval      I33OPE = *zeros
039700091223     C                   eval      I33CNU = 302
039800091223     C                   eval      I33NUM = 1
039900091223     C                   movel     TRUL33DS      KPJBU
040000091223     C                   call      'TRUL33R'
040100091223     C                   parm                    KPJBA
040200091223     C                   movel     KPJBU         TRUL33DS
040300091223     C                   if        O33ERR = *zeros
040400091223     C                   z-add     O33NRF        VABNSP
040500091223     C                   z-add     O33NRF        VATNSP
040600091223     C                   else
040700091223     C                   SETON                                        31
040800091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040900091223     C                             + ' ' + 'VABNSP VATNSP'
041000091223     C                   endif
041100120327     C                   endif
041200091223     C*
041300091223     C                   ENDSR
041400120524     C*----------------------------------------------------*
041500120524     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041600120524     C*----------------------------------------------------*
041700120524     C     IMPVAB        BEGSR
041800120524     C*
041900120524     C                   SETOFF                                       3132
042000120529     C*
042100120529     C                   MOVE(P)   vlrpoi        VABFGS
042200120529     C                   MOVE(P)   vlrpoi        VATFGS
042300120524     C*
042400120524     C* Reperimento campi NUMERICI
042500120524     C                   MOVEL     DATCOR        VABAAS
042600120524     C                   MOVEL     datcor        VATAAS
042700120524     C                   MOVE      DATCOR        VABMGS
042800120524     C                   clear                   PiDecChr
042900120524     C*
043000120524     C* Considerazioni sulla ragione sociale del destinatario da indicare
043100120524     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
043200120524     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043300120524     C     '@':'A'       XLATE     VABRSD        VABRSD
043400120524     C* ==
043500120524     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
043600120524     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
043700120524     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
043800120524     C                   EVAL      VABNOT=%trim(%subst(vindta:199:35))
043900120524     C                   EVAL      VABNT2=%trim(%subst(vindta:199+35:80-35))
044000120524     C                   EVAL      VABRMA=%trim(%subst(vindta:191:6))
044100120524     C                   IF        %trim(%subst(vindta:197:2)) = '01'
044200120524     C                   EVAL      VABCBO='2'
044300120524     C                   ELSE
044400120524     C                   EVAL      VABCBO='1'
044500120524     C                   ENDIF
044600120524     C* CCM
044700120524     C                   IF        %subst(vindta:1:11) <> *blanks AND
044800120524     C                             %subst(vindta:1:11) <> *all'0'
044900120524     C                   EVAL      PiStr=%trim(%subst(vindta:1:11))
045000120524     C                   EXSR      CHKNUM
045100120524     C                   IF        PiInt=*on
045200120524     C                   Z-ADD     PiVal         VABCCM
045300120524     C                   ELSE
045400120524     C                   SETON                                        31
045500120524     C                   Z-ADD     *zeros        VABCCM
045600120524     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045700120524     C                             + ' ' + 'VABCCM'
045800120524     C                   ENDIF
045900120524     C                   ENDIF
046000120524     C* CAD
046100120524     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
046200120524     C                   EXSR      CHKNUM
046300120524     C                   IF        PiInt=*on
046400120524     C                   Z-ADD     PiVal         Num5_0
046500120524     C                   MOVEL(p)  Num5_0        VABCAD
046600120524     C                   ELSE
046700120524     C                   SETON                                        32
046800120524     C                   EVAL      VABCAD = *zeros
046900120524     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047000120524     C                             + ' ' + 'VABCAD'
047100120524     C                   ENDIF
047200120524     C* NSP/RMN
047300120524     C                   EVAL      PiStr=%trim(%subst(vindta:191:6))
047400120524     C                   EXSR      CHKNUM
047500120524     C                   IF        PiInt=*on
047600120524     C                   Z-ADD     PiVal         VABNSP
047700120524     C                   Z-ADD     PiVal         VATNSP
047800120524     C                   Z-ADD     PiVal         VABRMN
047900120524     C                   ELSE
048000120524     C                   SETON                                        31
048100120524     C                   Z-ADD     *zeros        VABNSP
048200120524     C                   Z-ADD     *zeros        VABRMN
048300120524     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048400120524     C                             + ' ' + 'VABNSP VABRMN VATNSP'
048500120524     C                   ENDIF
048600120524     C* NCL
048700120524     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
048800120524     C                   EXSR      CHKNUM
048900120524     C                   IF        PiInt=*on
049000120524     C                   Z-ADD     PiVal         VABNCL
049100120524     C                   ELSE
049200120524     C                   SETON                                        32
049300120524     C                   Z-ADD     *zeros        VABNCL
049400120524     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049500120524     C                             + ' ' + 'VABNCL'
049600120524     C                   ENDIF
049700120524     C* PKB
049800120524     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
049900120524     C                   EXSR      CHKNUM
050000120524     C                   IF        PiNum=*on
050100120524     C                   Z-ADD     PiVal         VABPKB
050200120524     C                   ELSE
050300120524     C                   SETON                                        32
050400120524     C                   Z-ADD     *zeros        VABPKB
050500120524     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050600120524     C                             + ' ' + 'VABPKB'
050700120524     C                   ENDIF
050800120524     C* CAS
050900120524     C                   EVAL      PiStr=%trim(%subst(vindta:279:11))
051000120524     C                   EVAL      PiDecChr = '.'
051100120524     C                   EXSR      CHKNUM
051200120524     C                   IF        PiNum=*on
051300120524     C                   Z-ADD     PiVal         VABCAS
051400120524     C                   IF        VABCAS > 0
051500120524     C                   EVAL      FlgCAS = '1'
051600120524     C                   EVAL      VABVCA =  'EUR'
051700120524     C                   ENDIF
051800120524     C                   ELSE
051900120524     C                   SETON                                        32
052000120524     C                   Z-ADD     *zeros        VABCAS
052100120524     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052200120524     C                             + ' ' + 'VABCAS'
052300120524     C                   ENDIF
052400120524     C* IAS
052500120524     C                   EVAL      PiStr=%trim(%subst(vindta:292:11))
052600120524     C                   EVAL      PiDecChr = '.'
052700120524     C                   EXSR      CHKNUM
052800120524     C                   IF        PiNum=*on
052900120524     C                   Z-ADD     PiVal         VABIAS
053000120524     C                   IF        VABIAS > 0
053100120524     C                   EVAL      VABVAS =  'EUR'
053200120524     C                   ENDIF
053300120524     C                   ELSE
053400120524     C                   SETON                                        32
053500120524     C                   Z-ADD     *zeros        VABIAS
053600120524     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053700120524     C                             + ' ' + 'VABIAS'
053800120524     C                   ENDIF
053900120524     C*
054000120524     C* Considerazioni finali su CBO/CAS
054100120524     C                   IF        FlgCAS = '1'
054200120524     C                   IF        VABCBO = '1'
054300120524     C                   EVAL      VABCBO = '4'
054400120524     C                   ENDIF
054500120524     C                   IF        VABCBO = '2'
054600120524     C                   EVAL      VABCBO = '6'
054700120524     C                   ENDIF
054800120524     C                   ENDIF
054900120524     C*
055000120524     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055100120524     C                   EXSR      CHKIMPDIV
055200120524     C*
055300120524     C                   ENDSR
055400120524     C*----------------------------------------------------*
055500120524
055600120524
055700120524     C*----------------------------------------------------*
055800120524     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055900120524     C*----------------------------------------------------*
056000120524     C     CHKIMPDIV     BEGSR
056100120524     C*
056200120524     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056300120524     C                   Z-ADD     *zeros        wrkDec            9 9
056400120524     C*
056500120524     C* Come prima cosa effettuo considerazioni sulla divisa
056600120524     C                   IF        vabIAS > *zeros
056700120524     C                   IF        vabVAS <> 'EUR'
056800120524     C                   EVAL      vabVAS =  'ITL'
056900120524     C                   ENDIF
057000120524     C                   ENDIF
057100120524     C*
057200120524     C                   IF        vabCAS > *zeros
057300120524     C                   IF        vabVCA <> 'EUR'
057400120524     C                   EVAL      vabVCA =  'ITL'
057500120524     C                   ENDIF
057600120524     C                   ENDIF
057700120524     C*
057800120524     C                   IF        vabVMD > *zeros
057900120524     C                   IF        vabVAD <> 'EUR'
058000120524     C                   EVAL      vabVAD =  'ITL'
058100120524     C                   ENDIF
058200120524     C                   ENDIF
058300120524     C*
058400120524     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058500120524     C                   Z-ADD     vabIAS        wrkDec
058600120524     C                   IF        wrkDec > *zeros
058700120524     C                   IF        vabVAS = 'ITL'
058800120524     C                   EVAL      vabIAS = *zeros
058900120524     C                   ENDIF
059000120524     C                   ENDIF
059100120524     C*
059200120524     C* Stabilisco se il contrasegno ha decimali valorizzati
059300120524     C                   Z-ADD     vabCAS        wrkDec
059400120524     C                   IF        wrkDec > *zeros
059500120524     C                   IF        vabVCA = 'ITL'
059600120524     C                   EVAL      vabCAS = *zeros
059700120524     C                   ENDIF
059800120524     C                   ENDIF
059900120524     C*
060000120524     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060100120524     C                   Z-ADD     vabVMD        wrkDec
060200120524     C                   IF        wrkDec > *zeros
060300120524     C                   IF        vabVAD = 'ITL'
060400120524     C                   EVAL      vabVMD = *zeros
060500120524     C                   ENDIF
060600120524     C                   ENDIF
060700120524     C*
060800120524     C                   ENDSR
060900120524     C***
061000100127     C*----------------------------------------------------*
061100100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
061200100127     C*----------------------------------------------------*
061300100127     C     WRIVAT_A      BEGSR
061400100127     C*
061500120327     C* NON GESTITO TIPO RCD A
061600120327     C*
061700100127     C* Valorizzo l buffer di scrittura del FIVAT
061800100127     C* - TIPO RECORD "A"
061900101119     C***                eval      VATTRC = 'A'
062000120327     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yy))
062100101119     C***                exsr      wrivat
062200100127     C*
062300100127     C                   ENDSR
062400010201     C*----------------------------------------------------*
062500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
062600010201     C*----------------------------------------------------*
062700071003     C     WRIVAT_B      BEGSR
062800010201     C*
062900021113     C* Valorizzo l buffer di scrittura del FIVAT
063000070928     C* - TIPO RECORD "B"
063100120524     C***                eval      VATTRC = 'B'
063200120524     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yy))
063300120524     C***                exsr      wrivat
063400010201     C*
063500010201     C                   ENDSR
063600071003     C*----------------------------------------------------*
063700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063800071003     C*----------------------------------------------------*
063900071003     C     WRIVAT_E      BEGSR
064000071003     C*
064100071003     C* Valorizzo l buffer di scrittura del FIVAT
064200071003     C* - TIPO RECORD "E"
064300120524     C***
064400120524     C***                eval      VATTRC = 'E'
064500120524     C**i CHI SONO vanno sviluppati prendendo il valore passato come template
064600120524     C**togliendo le ultime 3 cifre e sostituendole con lo sviluppo
064700120524     C**sviluppo che parte da 1 e va in relazione al numero colli
064800120524     C***                MOVEL     *blanks       wWhoIsRef        30
064900120524     C***                eval      wWhoIsRef = %trim(%subst(vindta:492:30))
065000120524     C***                eval      wWhoIsRef = %subst(wWhoIsRef:1:
065100120524     C***                           %len(%trim(wWhoIsRef)) - 6)
065200120524     C***                exsr      wrivat
065300120524     C***
065400120524     C**Sviluppati i "CHI SONO" in relazione al numero colli
065500120524     C***
065600120524     C**Inizializzo progressivo collo su totale colli della spedizione
065700120524     C***                Z-ADD     1             wNumColloDA       3 0
065800120524     C***                MOVEL     *blanks       wNumColloCor      3
065900120524     C***                MOVEL     *blanks       wWhoIs            6
066000120524     C***
066100120524     C***                DOW       wNumColloDA <= VABNCL
066200120524     C***
066300120524     C***                MOVEL(P)  wNumColloDA   wNumColloCor
066400120524     C***                EVAL      VATNOT =
066500120524     C***                           %trim(wWhoIsRef) + %editc(wNumColloDA:'X')
066600120524     C***                exsr      wrivat
066700120524     C***                add       1             §CTROKVT
066800120524     C***
066900120524     C***                EVAL      wNumColloDA = wNumColloDA + 1
067000120524     C***                ENDDO
067100071003     C*
067200071003     C                   ENDSR
067300120524     C*----------------------------------------------------*
067400120524     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I"
067500120524     C*----------------------------------------------------*
067600120524     C     WRIVAT_I      BEGSR
067700120524     C*
067800120524     C* Valorizzo l buffer di scrittura del EDIVAT
067900120524     C* - TIPO RECORD "I"
068000120524     C                   if        %trim(%subst(vindta:386:35)) <> *blank
068100120524     C                   eval      VATTRC = 'I'
068200120524     C                   eval      VATNOT = %trim(%subst(vindta:386:35))
068300120524     C                   exsr      wrivat
068400120524     C                   endif
068500120524     C*
068600120524     C                   ENDSR
068700100127     C*----------------------------------------------------*
068800100127     C*  SCARICO BUFFER FIVAT
068900100127     C*----------------------------------------------------*
069000100127     C     WRIVAT        BEGSR
069100100127     C*
069200100127     C                   if        vatNOT <> *blanks
069300100127     C                   ADD       1             §CTROKVT
069400100127     C                   write     FIVAT000
069500100127     C                   endif
069600100127     C*
069700100127     C                   ENDSR
069800020904
069900020904
070000010202     C*----------------------------------------------------*
070100100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
070200010202     C*----------------------------------------------------*
070300100525     C     PREVASX       BEGSR
070400100525     C*
070500021113     C* Compongo il nome del membro da dare al FIVATWWR
070600010202     C                   eval      parmbr = vlrhdl
070700010202     C                   movel     'M'           parmbr
070800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
070900010202     C                   eval      paropz = '1'
071000100525     C*
071100010202     C* Effettuo la chiamata al CLLE preposto
071200100525     C  N51              call(e)   'TITVVTC'
071300010202     C                   parm                    parccm
071400010202     C                   parm                    parmbr
071500010202     C                   parm                    paropz
071600100525     C   51              call(e)   'TITVVBC'
071700100525     C                   parm                    parccm
071800100525     C                   parm                    parmbr
071900100525     C                   parm                    paropz
072000100525     C*
072100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072200010202     C                   if        %error
072300010202     C                   movel     '1'           chkcall
072400010202     C                   else
072500010202     C                   movel     '0'           chkcall
072600010202     C                   endif
072700010202     C*
072800010202     C                   ENDSR
072900000801     C*----------------------------------------------------*
073000000801     C*  CONTROLLO NUMERICITA' CAMPI
073100000801     C*----------------------------------------------------*
073200000801     C     CHKNUM        BEGSR
073300081113     C*
073400081113     C                   IF        PiDecChr = *blanks
073500110408     C                   EVAL      PiDecChr = '.'
073600081113     C                   ENDIF
073700091223     C*
073800081113     C                   callp(e)  UBISNUM_Check(PiStr
073900081113     C                                          :PiDecChr
074000081113     C                                          :PiVal
074100081113     C                                          :PiNum
074200081113     C                                          :PiInt)
074300081113     C*
074400000801     C                   IF        %error
074500000801     C                   EVAL      PiInt=*off
074600000801     C                   ENDIF
074700000801     C*
074800000801     C                   ENDSR
074900000801     C***
075000011113
075100011113
075200000801
075300000801
075400990920      /TITLE Invio dei dati al punto operativo.
075500010202     C     invio         BEGSR
075600990920     C*
075700021113     C* 1° invio FIVAT
075800010201     C                   reset                   dscmz
075900110411     C                   move      vatfgs        cmzdst
076000021113     C                   eval      cmzfld = 'FIVATWWR'
076100010201     C                   eval      cmzmbd = vlrhdl
076200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076300021009     C***                if        prmfir = *blanks
076400021113     C                   eval      cmzfla = 'FIVAT00F'
076500021113     C                   eval      cmzmba = 'FIVAT00F'
076600021009     C***                else
076700021009     C***                eval      cmzfla = prmfir
076800021009     C***                eval      cmzmba = prmfir
076900021009     C***                endif
077000010201     C                   eval      cmznrr = *zeros
077100020305     C                   move      §ctrokvt      cmznrr
077200021018     C                   eval      cmzlba = vlrfl1
077300010201     C                   call(e)   'TIS711C'
077400010201     C                   parm                    dscmz
077500010201     C                   parm      *blanks       esito
077600010205     C                   if        %error
077700010205     C                             or cmzerr = '1'
077800010205     C                             or esito  = '1'
077900010205     C                   eval      wrkesito = '3'
078000010205     C                   else
078100010201     C*
078200021113     C* 2° invio FIVAB
078300100525     C                   if        *in51 = *off
078400010201     C                   reset                   dscmz
078500110411     C                   move      vabfgs        cmzdst
078600010201     C                   eval      cmzfld = vlrfou
078700010201     C                   eval      cmzmbd = vlrhdl
078800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078900021009     C***                if        prmfir = *blanks
079000021113     C                   eval      cmzfla = 'FIVAB00F'
079100021113     C                   eval      cmzmba = 'FIVAB00F'
079200021009     C***                else
079300021009     C***                eval      cmzfla = prmfir
079400021009     C***                eval      cmzmba = prmfir
079500021009     C***                endif
079600010201     C                   eval      cmznrr = *zeros
079700010201     C                   move      §ctrokvb      cmznrr
079800021018     C                   eval      cmzlba = vlrfl1
079900010201     C                   call(e)   'TIS711C'
080000010201     C                   parm                    dscmz
080100010201     C                   parm      *blanks       esito
080200010201     C                   if        %error
080300010201     C                             or cmzerr = '1'
080400010201     C                             or esito  = '1'
080500010201     C                   eval      wrkesito = '3'
080600010201     C                   endif
080700100525     C                   endif
080800010205     C                   endif
080900990920     C*
081000000613     C                   ENDSR
081100000613     C***
081200070411
081300110627     C     *pssr         BEGSR
081400070411     C*
081500110627     C                   if        %open(tivin00r)
081600110627     C                   close     tivin00r
081700110627     C                   endif
081800110627     C                   if        %open(fivabwwr)
081900110627     C                   close     fivabwwr
082000110627     C                   endif
082100110627     C                   if        %open(fivatwwr)
082200110627     C                   close     fivatwwr
082300110627     C                   endif
082400110627     C*
082500110627     C* Effettuo la chiamata al CLLE preposto
082600110627     C  N51              call(e)   'TITVVTC'
082700110627     C                   parm                    parccm
082800110627     C                   parm                    parmbr
082900110627     C                   parm      '2'           paropz
083000110627     C   51              call(e)   'TITVVBC'
083100110627     C                   parm                    parccm
083200110627     C                   parm                    parmbr
083300110627     C                   parm      '2'           paropz
083400110627     C*
083500110627     C                   eval      wrkesito = '2'
083600110627     C*
083700110627     C                   seton                                        LR
083800110627     C*
083900110627     C                   ENDSR     '*CANCL'
084000070411     C***
084100070411
084200990910
084300000613     C     *inzsr        BEGSR
084400990910     C*
084500990910     C     *entry        plist
084600990920     C                   parm                    tivlrds
084700990921     C                   parm      wrkesito      esito
084800000724     C                   parm                    prmlit
084900000710     C                   parm                    prmfir
085000000613     C*
085100000830     C* CALCOLA LA DATA CORRENTE
085200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085300091223     C                   eval      datcor = %dec(%date() : *ISO)
085400000830     C*
085500000613     C                   ENDSR
085600000613     C***
