000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300120209
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
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
001500070730     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100111227     D depspe          s              7    INZ(*blanks)
002200111227     D curspe          s              7    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800070530     D Num5_0          s              5  0
002900000830
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500101007     D  VINDTA                     2048
003600101007     D  VINFLG                        1
003700101007     D  VINSPE                       10
003800101007     D  VINRRN                        8  0
003900020725     D*
004000080923     D*------------------
004100080923     D* DS REPERIMENTO NUMERATORE
004200080923     D*------------------
004300080923     D trul33ds      e ds                  inz
004400080923     D*------------------
004500080923     D* DS ARCHITETTURA
004600080923     D*------------------
004700080923     D kpjba         e ds                  inz
004800080923     D*------------------
004900111205
005000111205
005100111205     D*-------------------
005200111205     D* COSTANTI
005300111205     D*-------------------
005400111205     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm���-    *alfabeto minus.
005500111205     D                                     ���')
005600111205     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005700111205     D                                     EEU')
005800081113
005900081113     D*------------------
006000081113     D* LINKING A DEFINIZIONI ESTERNE
006100081113     D*------------------
006200100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006400081113
006500990908
006600010201
006700010201
006800080117     C*
006900080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007000080117     C
007100080117     C/EXEC SQL
007200080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007300080117     C/END-EXEC
007400080117     C
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800000613     C*
007900111228     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008100000613     C*
008200010202     C* Effettuo la chiamata al CLLE preposto
008300050128     C                   call(e)   'TITVVTC'
008400010202     C                   parm                    parccm
008500010202     C                   parm                    parmbr
008600010202     C                   parm      '2'           paropz
008700070730     C*
008800070730     C* Effettuo lancio TISI95 solo x chiusura
008900070730     C                   CLEAR                   TISI95DS
009000070730     C                   EVAL      I95TLA = 'C'
009100070730     C                   CALL      'TISI95R'
009200070730     C                   PARM                    TISI95DS
009300000616     C*
009400000801     C
009500010201     C                   seton                                        LR
009600990908
009700000801
009800910830     C*--------------------------------------------------------
009900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010000910830     C*--------------------------------------------------------
010100070530     C     RWFILE        BEGSR
010200990910     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600021113     C                   if        not %open(fivabwwr)
010700021113     C                   open      fivabwwr
010800990914     C                   endif
010900070530     C*
011000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011100020305     C                   exsr      prevat
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400010202     C*
011500021113     C                   if        not %open(fivatwwr)
011600021113     C                   open      fivatwwr
011700010201     C                   endif
011800080117     C*
011900080117     C                   EXSR      INZVAR
012000080117     C                   EXSR      DEFCAM
012100990910     C*
012200010201     C                   clear                   �CTROKVB          5 0
012300020305     C                   clear                   �CTROKVT          5 0
012400000801     C                   clear                   �CTRMO            5 0
012500000801     C                   clear                   �CTRNO            5 0
012600990910     C*
012700020725     C/EXEC SQL
012800020725     C+ declare C1 cursor for select
012900111227     C+ vindta, vinflg, substr(vindta, 867, 7) as sped, rrn(tivin00r)
013000060223     C+ from tivin00r
013100060223     C+ order by sped
013200020725     C+ for read only
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600020725     C+ open C1
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000070530     C+ Fetch C1 into :INPUT_DS
014100020725     C/END-EXEC
014200020725     C*
014300020725     C                   dow       sqlcod = *zeros
014400990913     C*
014500020725     C                   if        vindta > *blanks
014600000613     C                   add       1             rrnum
014700111228
014800111228      * salto il primo rcd in quanto non valido
014900111228     c                   IF        rrnum > 1
015000000801     C*
015100020725     C                   if        vinflg = *blanks
015200020725     C                             or vinflg = '0'
015300020725     C                             or vinflg = '2'
015400000801     C*
015500020725     C                   clear                   x_vinmsg
015600020725     C                   eval      x_vinflg = '1'
015700111205     C*
015800111205     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
015900111205     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016000010305     C*
016100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016200111227     C                   EVAL      curspe=%trim(%subst(vindta:867:7))
016300010305     C*
016400071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016500080923     C                   if        *in50 = *off
016600111228     C                   exsr      inzvar
016700111228     C                   exsr      defcam
016800071003     C                   exsr      impvab
016900071003     C                   exsr      wrivab
017000111228     C                   exsr      wrivat_b                                     => carico VAT
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400111228     C                   exsr      inzvar
017500111228     C                   exsr      defcam
017600071009     C                   exsr      impvab
017700071009     C                   exsr      wrivab
017800111227     C                   exsr      wrivat_b                                     => carico VAT
017900071009     C                   exsr      wrivat_e                                     => carico VAT
018000071009     C                   else
018100071009     C*
018200010305     C                   if        depspe = *blanks                             => 1� giro
018300010305     C                   eval      depspe = curspe                              => memorizz. spediz
018400080117     C                   clear                   tivinds
018500111228     C                   exsr      inzvar
018600111228     C                   exsr      defcam
018700020305     C                   exsr      impvab
018800111227     C   50              exsr      wrivat_b                                     => carico VAT
018900071003     C   50              exsr      wrivat_e                                     => carico VAT
019000010305     C                   else
019100020725     C                   if        depspe <> curspe                             => rottura di spediz
019200010305     C                   eval      depspe = curspe                              => memorizz. spediz
019300070928     C                   exsr      wrivab
019400080117     C                   clear                   tivinds
019500111228     C                   exsr      inzvar
019600111228     C                   exsr      defcam
019700020305     C                   exsr      impvab
019800111227     C   50              exsr      wrivat_b                                     => carico VAT
019900071003     C   50              exsr      wrivat_e                                     => carico VAT
020000020305     C                   else                                                   => x stessa spediz
020100111228     C                   exsr      impvab
020200111227     C   50              exsr      wrivat_b                                     => carico VAT
020300071003     C   50              exsr      wrivat_e                                     => carico VAT
020400010305     C                   endif
020500010305     C                   endif
020600010305     C                   endif
020700071003     C                   endif
020800071009     C                   endif
020900111228     c                   ENDIF
021000000905     C*
021100000905     C                   else
021200020725     C                   eval      x_vinflg = '1'
021300000905     C                   endif
021400000905     C*
021500020725     C     VINRRN        chain     tivin000
021600020725     C                   if        %found(tivin00r)
021700020725     C                   eval      y_vinflg = x_vinflg
021800020725     C                   eval      y_vinmsg = x_vinmsg
021900020725     C                   update    tivin000
022000020725     C                   endif
022100020725     C*
022200020725     C/EXEC SQL
022300070530     C+ Fetch C1 into :INPUT_DS
022400020725     C/END-EXEC
022500020725     C*
022600020725     C                   enddo
022700020725     C*
022800020725     C/EXEC SQL
022900020725     C+ close C1
023000020725     C/END-EXEC
023100000905     C*
023200020305     C* Scarico i VAB rimasti "in sospeso"
023300071009     C                   if        wDISK = 'C'
023400071009     C                   exsr      wrivab
023500071009     C                   endif
023600010202     C*
023700010202     C                   endif
023800990910
023900990910     C* Se non ci sono record con errori ...
024000000710     C                   if        �ctrno = 0
024100990910     C* ... restituisco esito OK.
024200990921     C                   eval      wrkesito = '0'
024300990910     C                   else
024400010201     C                   if        �ctrokvb > 0
024500990921     C                   eval      wrkesito = '1'
024600000710     C                   else
024700000710     C                   eval      wrkesito = '2'
024800990910     C                   endif
024900000710     C                   endif
025000990910     C*
025100990914     C                   if        %open(tivin00r)
025200990908     C                   close     tivin00r
025300990914     C                   endif
025400021113     C                   if        %open(fivabwwr)
025500021113     C                   close     fivabwwr
025600990914     C                   endif
025700021113     C                   if        %open(fivatwwr)
025800021113     C                   close     fivatwwr
025900010201     C                   endif
026000990910     C*
026100010201     C                   if        �ctrokvb > 0
026200000724     C                             and vlrpoi <> *zeros
026300010202     C                   exsr      invio
026400990920     C                   endif
026500990920     C*
026600910830     C                   ENDSR
026700000613     C***
026800010305
026900010305     C*----------------------------------------------------*
027000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027100010305     C*----------------------------------------------------*
027200020305     C     WRIVAB        BEGSR
027300070730     C*
027400071003     C* Considerazioni finali
027500071003     C                   if        VABRMA = *blanks
027600071003     C                   movel(P)  VABRMN        VABRMA
027700071003     C                   endif
027800071003     C*
027900021113     C                   write     fivab000                                     => scarico il VAB
028000010305     C*
028100010305     C                   ENDSR
028200990920
028300000801     C*----------------------------------------------------*
028400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028500000801     C*----------------------------------------------------*
028600010201     C     INZVAR        BEGSR
028700000801     C*
028800010201     C                   Z-ADD     *zeros        Num5_0
028900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029000020725     C                   MOVEL     '0'           FlgCAS            1
029100000801     C*
029200000801     C                   ENDSR
029300000801     C*----------------------------------------------------*
029400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029500000801     C*----------------------------------------------------*
029600020904     C     DEFCAM        BEGSR
029700080923     C*
029800080923     C                   SETOFF                                       5051
029900000801     C*
030000021113     C                   CLEAR                   FIVAB000
030100021113     C                   CLEAR                   FIVAT000
030200070730     C* Imposto i valori di default...
030300111227     C                   EVAL      VABCCM = 0896476
030400111227     C                   EVAL      VATCCM = 0896476
030500111227     C                   EVAL      VABLNP = 089
030600111227     C                   EVAL      VATLNP = 089
030700101007     C                   EVAL      VABCTR = 000
030800070730     C                   EVAL      VABCBO = '1'
030900111227     C                   EVAL      VABTSP = 'C'
031000070222     C* ... e poi verifico se sono stati passati come parametri
031100070222     C                   IF        vlrppt > *blanks
031200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABCCM
031700070222     C                   Z-ADD     PiVal         VATCCM
031800070222     C                   ENDIF
031900070222     C                   ENDIF
032000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032200070222     C                   EXSR      CHKNUM
032300070222     C                   IF        PiInt=*on
032400070222     C                   Z-ADD     PiVal         VABLNP
032500070222     C                   Z-ADD     PiVal         VATLNP
032600070222     C                   ENDIF
032700070222     C                   ENDIF
032800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033000070222     C                   EXSR      CHKNUM
033100070222     C                   IF        PiInt=*on
033200070222     C                   Z-ADD     PiVal         VABCTR
033300070222     C                   ENDIF
033400070928     C                   ENDIF
033500071009     C                   MOVEL     *blanks       wDISK             1
033600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033700071009     C                   IF        wDISK <> *blanks
033800070928     C                   SETON                                        50
033900070222     C                   ENDIF
034000080923     C                   IF        %subst(vlrppt:15:1) = 'S'
034100080923     C                   SETON                                        51
034200080923     C                   ENDIF
034300070222     C                   ENDIF
034400071009     C*
034500071009     C   50              EVAL      VABCTM = '7Q'
034600000801     C*
034700000801     C                   ENDSR
034800000801     C*----------------------------------------------------*
034900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035000000801     C*----------------------------------------------------*
035100010201     C     IMPVAB        BEGSR
035200070530     C*
035300070530     C                   SETOFF                                       3132
035400070928     C*
035500070928     C                   MOVE(P)   vlrpoi        VABFGS
035600070928     C                   MOVE(P)   vlrpoi        VATFGS
035700070928     C*
035800070928     C                   MOVEL     datcor        VABAAS
035900070928     C                   MOVEL     datcor        VATAAS
036000070928     C                   MOVE      datcor        VABMGS
036100111227     C                   EVAL      VABRSD=%trim(%subst(vindta:201:35))
036200070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'� diventa A ==
036300070928     C     '@':'A'       XLATE     VABRSD        VABRSD
036400070928     C* ==
036500111227     C                   EVAL      VABIND=%trim(%subst(vindta:96:35))
036600111227     C                   EVAL      VABLOD=%trim(%subst(vindta:281:35))
036700111227     C                   EVAL      VABCAD=%trim(%subst(vindta:271:10))
036800111227     C                   EVAL      VABRMA=%trim(%subst(vindta:1:15))
036900111227     C                   EVAL      VABRD2=%trim(%subst(vindta:61:35))
037000080923     C* RMN
037100111227     C                   EVAL      PiStr=%trim(%subst(vindta:867:7))
037200080923     C                   EXSR      CHKNUM
037300080923     C                   IF        PiInt=*on
037400080923     C                   Z-ADD     PiVal         VABRMN
037500080923     C                   ELSE
037600080923     C                   SETON                                        32
037700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037800080923     C                             + ' ' + 'VABRMN'
037900080923     C                   ENDIF
038000080923     C* NSP
038100080923     C                   IF        *in51 = *on
038200080923     C* NSP => Stacco un numeratore da AZNUM
038300080923     C                   clear                   TRUL33DS
038400080923     C                   eval      I33OPE = *zeros
038500080923     C                   eval      I33CNU = 302
038600080923     C                   eval      I33NUM = 1
038700080923     C                   movel     TRUL33DS      KPJBU
038800080923     C                   call      'TRUL33R'
038900080923     C                   parm                    KPJBA
039000080923     C                   movel     KPJBU         TRUL33DS
039100080923     C                   if        O33ERR = *zeros
039200080923     C                   z-add     O33NRF        VABNSP
039300080923     C                   z-add     O33NRF        VATNSP
039400080923     C                   else
039500080923     C                   SETON                                        31
039600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039700080923     C                             + ' ' + 'VABNSP VATNSP'
039800080923     C                   endif
039900080923     C                   ELSE
040000111228     C                   EVAL      PiStr=%trim(%subst(vindta:867:7))
040100080923     C                   EXSR      CHKNUM
040200080923     C                   IF        PiInt=*on
040300080923     C                   Z-ADD     PiVal         VABNSP
040400080923     C                   Z-ADD     PiVal         VATNSP
040500080923     C                   ELSE
040600080923     C                   SETON                                        31
040700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800080923     C                             + ' ' + 'VABNSP VATNSP'
040900080923     C                   ENDIF
041000080923     C                   ENDIF
041100071003     C* NCL
041200111205     C                   ADD       1             VABNCL
041300071003     C* PKB
041400111227     C                   EVAL      PiStr=%trim(%subst(vindta:38:8))
041500070928     C                   EXSR      CHKNUM
041600070928     C                   IF        PiNum=*on
041700111227     C                   EVAL      PiVal=PiVal/100                              gestisco 2 dec.
041800111228     C                   ADD(H)    PiVal         VABPKB
041900070928     C                   ELSE
042000070928     C                   SETON                                        32
042100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042200070928     C                             + ' ' + 'VABPKB'
042300070928     C                   ENDIF
042400111227     C* CAS
042500111227      * se alla 1028 � impostato '1' vuol dire che � in contrassegno
042600111227     C                   IF        %subst(vindta:1028:1) = '1'
042700111227     C                   EVAL      FlgCAS = '1'
042800111228     C                   EVAL      VABvca = 'EUR'
042900111227     C                   EVAL      PiStr=%trim(%subst(vindta:1019:9))
043000111227     C                   EXSR      CHKNUM
043100111227     C                   IF        PiNum=*on
043200111227     C                   Z-ADD     PiVal         VABCAS
043300111227     C                   ELSE
043400111227     C                   SETON                                        32
043500111227     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043600111227     C                             + ' ' + 'VABCAS'
043700111227     C                   ENDIF
043800111227     C                   ENDIF
043900111205     C*
044000111205     C* Se provincia nn valorizzata la reperisco
044100111205     C* tramite TISI95R a seconda dei dati d instradamento presenti
044200111205     C                   IF        VABNZD  = *blanks AND
044300111205     C                             VABPRD  = *blanks AND
044400111205     C                             VABCAD <> *blanks
044500111205     C                   CLEAR                   TISI95DS
044600111205     C                   EVAL      I95TCN = '3'
044700111205     C                   Z-ADD     datcor        I95DAT
044800111205     C                   EVAL      I95NAR = VABNZD
044900111205     C                   EVAL      I95CAP = VABCAD
045000111205     C                   EVAL      I95LOC = VABLOD
045100111205     C                   CALL      'TISI95R'
045200111205     C                   PARM                    TISI95DS
045300111205     C                   EVAL      VABPRD = O95PRV
045400111205     C                   ENDIF
045500070730     C*
045600070730     C* Considerazioni finali su CBO/CAS
045700070730     C                   IF        FlgCAS = '1'
045800070730     C                   IF        VABCBO = '1'
045900070730     C                   EVAL      VABCBO = '4'
046000070730     C                   ENDIF
046100070730     C                   IF        VABCBO = '2'
046200070730     C                   EVAL      VABCBO = '6'
046300070730     C                   ENDIF
046400070730     C                   ENDIF
046500010202     C*
046600000801     C* Ebbene...
046700000801     C                   ADD       1             �CTRMO
046800070530     C                   IF        *in31 <> *zeros OR
046900070530     C                             *in32 <> *zeros
047000000801     C                   ADD       1             �CTRNO
047100020725     C                   EVAL      x_vinflg = '2'
047200000801     C                   ELSE
047300010201     C                   ADD       1             �CTROKVB
047400000801     C                   ENDIF
047500000801     C*
047600000801     C                   ENDSR
047700111227     C*----------------------------------------------------*
047800111227     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
047900111227     C*----------------------------------------------------*
048000111227     C     WRIVAT_B      BEGSR
048100111227     C*
048200111227     C                   eval      vatAAS = vabAAS
048300111227     C                   eval      vatLNP = vabLNP
048400111227     C                   eval      vatNRS = vabNRS
048500111227     C                   eval      vatNSP = vabNSP
048600111227     C*
048700111227     C* Valorizzo il buffer di scrittura del FIVAT
048800111227     C* - TIPO RECORD "B"
048900111227     C                   if        %subst(vindta:374:10) <> *blanks
049000111227     C                   eval      VATTRC = 'B'
049100111227     C                   eval      VATNOT = %trim(%subst(vindta:374:10))
049200111227     C                   write     FIVAT000
049300111227     C                   endif
049400111227     C*
049500111227     C                   ENDSR
049600111227
049700071003     C*----------------------------------------------------*
049800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
049900071003     C*----------------------------------------------------*
050000071003     C     WRIVAT_E      BEGSR
050100090204     C*
050200090204     C                   eval      vatAAS = vabAAS
050300090204     C                   eval      vatLNP = vabLNP
050400090204     C                   eval      vatNRS = vabNRS
050500090204     C                   eval      vatNSP = vabNSP
050600071003     C*
050700101007     C* Valorizzo il buffer di scrittura del FIVAT
050800071003     C* - TIPO RECORD "E"
050900120103     C                   if        %subst(vindta:920:21) <> *blanks
051000071003     C                   eval      VATTRC = 'E'
051100120103     C                   eval      VATNOT = %trim(%subst(vindta:920:21))
051200071003     C                   write     FIVAT000
051300111205     C                   endif
051400071003     C*
051500071003     C                   ENDSR
051600020904
051700010202     C*----------------------------------------------------*
051800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051900010202     C*----------------------------------------------------*
052000020305     C     PREVAT        BEGSR
052100010202     C*
052200021113     C* Compongo il nome del membro da dare al FIVATWWR
052300010202     C                   eval      parmbr = vlrhdl
052400010202     C                   movel     'M'           parmbr
052500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
052600010202     C                   eval      paropz = '1'
052700010202     C* Effettuo la chiamata al CLLE preposto
052800050128     C                   call(e)   'TITVVTC'
052900010202     C                   parm                    parccm
053000010202     C                   parm                    parmbr
053100010202     C                   parm                    paropz
053200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053300010202     C                   if        %error
053400010202     C                   movel     '1'           chkcall
053500010202     C                   else
053600010202     C                   movel     '0'           chkcall
053700010202     C                   endif
053800010202     C*
053900010202     C                   ENDSR
054000000801     C*----------------------------------------------------*
054100000801     C*  CONTROLLO NUMERICITA' CAMPI
054200000801     C*----------------------------------------------------*
054300000801     C     CHKNUM        BEGSR
054400081113     C*
054500081113     C                   IF        PiDecChr = *blanks
054600110223     C                   EVAL      PiDecChr = ','
054700081113     C                   ENDIF
054800081113     C*
054900081113     C                   callp(e)  UBISNUM_Check(PiStr
055000081113     C                                          :PiDecChr
055100081113     C                                          :PiVal
055200081113     C                                          :PiNum
055300081113     C                                          :PiInt)
055400081113     C*
055500000801     C                   IF        %error
055600000801     C                   EVAL      PiInt=*off
055700000801     C                   ENDIF
055800000801     C*
055900000801     C                   ENDSR
056000000801     C***
056100000801
056200011113
056300000801
056400000801
056500990920      /TITLE Invio dei dati al punto operativo.
056600010202     C     invio         BEGSR
056700990920     C*
056800021113     C* 1� invio FIVAT
056900010201     C                   reset                   dscmz
057000010201     C                   move      vlrpoi        cmzdst
057100021113     C                   eval      cmzfld = 'FIVATWWR'
057200010201     C                   eval      cmzmbd = vlrhdl
057300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057400021009     C***                if        prmfir = *blanks
057500021113     C                   eval      cmzfla = 'FIVAT00F'
057600021113     C                   eval      cmzmba = 'FIVAT00F'
057700021009     C***                else
057800021009     C***                eval      cmzfla = prmfir
057900021009     C***                eval      cmzmba = prmfir
058000021009     C***                endif
058100010201     C                   eval      cmznrr = *zeros
058200020305     C                   move      �ctrokvt      cmznrr
058300021018     C                   eval      cmzlba = vlrfl1
058400010201     C                   call(e)   'TIS711C'
058500010201     C                   parm                    dscmz
058600010201     C                   parm      *blanks       esito
058700010205     C                   if        %error
058800010205     C                             or cmzerr = '1'
058900010205     C                             or esito  = '1'
059000010205     C                   eval      wrkesito = '3'
059100010205     C                   else
059200010201     C*
059300021113     C* 2� invio FIVAB
059400010201     C                   reset                   dscmz
059500010201     C                   move      vlrpoi        cmzdst
059600010201     C                   eval      cmzfld = vlrfou
059700010201     C                   eval      cmzmbd = vlrhdl
059800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059900021009     C***                if        prmfir = *blanks
060000021113     C                   eval      cmzfla = 'FIVAB00F'
060100021113     C                   eval      cmzmba = 'FIVAB00F'
060200021009     C***                else
060300021009     C***                eval      cmzfla = prmfir
060400021009     C***                eval      cmzmba = prmfir
060500021009     C***                endif
060600010201     C                   eval      cmznrr = *zeros
060700010201     C                   move      �ctrokvb      cmznrr
060800021018     C                   eval      cmzlba = vlrfl1
060900010201     C                   call(e)   'TIS711C'
061000010201     C                   parm                    dscmz
061100010201     C                   parm      *blanks       esito
061200010201     C                   if        %error
061300010201     C                             or cmzerr = '1'
061400010201     C                             or esito  = '1'
061500010201     C                   eval      wrkesito = '3'
061600010201     C                   endif
061700010205     C                   endif
061800990920     C*
061900000613     C                   ENDSR
062000000613     C***
062100070411
062200070411     C     *pssr         BEGSR
062300070411     C*
062400070411     C                   if        %open(tivin00r)
062500070411     C                   close     tivin00r
062600070411     C                   endif
062700070411     C                   if        %open(fivabwwr)
062800070411     C                   close     fivabwwr
062900070411     C                   endif
063000070411     C                   if        %open(fivatwwr)
063100070411     C                   close     fivatwwr
063200070411     C                   endif
063300070411     C*
063400070411     C* Effettuo la chiamata al CLLE preposto
063500070411     C                   call(e)   'TITVVTC'
063600070411     C                   parm                    parccm
063700070411     C                   parm                    parmbr
063800070411     C                   parm      '2'           paropz
063900070411     C*
064000070411     C                   eval      wrkesito = '2'
064100070411     C*
064200070411     C                   seton                                        LR
064300070411     C*
064400070411     C                   ENDSR     '*CANCL'
064500070411     C***
064600070411
064700990910
064800000613     C     *inzsr        BEGSR
064900990910     C*
065000990910     C     *entry        plist
065100990920     C                   parm                    tivlrds
065200990921     C                   parm      wrkesito      esito
065300000724     C                   parm                    prmlit
065400000710     C                   parm                    prmfir
065500000613     C*
065600000830     C* CALCOLA LA DATA CORRENTE
065700100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065800100324     C                   eval      datcor = %dec(%date() : *ISO)
065900000830     C*
066000000613     C                   ENDSR
