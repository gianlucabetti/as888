000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100310     Ftitv1v8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100310     Ftitv1v8ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600070730     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600100310     D depspe          s             10    INZ(*blanks)
003700100310     D curspe          s             10    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300091223     D Num5_0          s              5  0
004400020725
004500020725     D*------------------
004600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004700020725     D*------------------
004800070530     D INPUT_DS        DS                  INZ
004900020725     D  VINDTA                 1   2048
005000020725     D  VINFLG              2049   2049
005100020725     D  VINSPE              2050   2059
005200020725     D  VINRRN              2060   2067  0
005300020725     D*
005400081113
005500091223     D*------------------
005600091223     D* DS REPERIMENTO NUMERATORE
005700091223     D*------------------
005800100309     D trul33ds      e ds                  inz
005900091223     D*------------------
006000091223     D* DS ARCHITETTURA
006100091223     D*------------------
006200091223     D kpjba         e ds                  inz
006300091223
006400081113
006500081113     D*------------------
006600081113     D* LINKING A DEFINIZIONI ESTERNE
006700081113     D*------------------
006800081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006900090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007000081113
007100990908
007200010201
007300010201
007400080117     C*
007500080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007600080117     C
007700080117     C/EXEC SQL
007800080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007900080117     C/END-EXEC
008000080117     C
008100990915     C                   time                    wrkdata
008200990915     C                   time                    wrkora
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000724     C*
008700000724     C* SE OCCORRE SPEDIRE IN FILIALE
008800000724     C                   if        vlrpoi <> *zeros
008900000724     C*
009000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200000724     C     vlrpoi        chain     azorg01l
009300000724     C                   if        %found
009400000616     C                   movel(p)  CMD(1)        depcmd
009500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009600000616     C*
009700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000616     C                   PARM                    depcmd
010100000616     C                   PARM                    LENGH
010200000724     C*
010300000724     C                   endif
010400000724     C                   endif
010500000616     C*
010600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010700000616     C                   movel(p)  CMD(2)        depcmd
010800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010900000616     C*
011000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000616     C                   PARM                    depcmd
011400000616     C                   PARM                    LENGH
011500000616     C*
011600100310     C                   if        not %open(titv1v8ps)
011700100310     C                   open      titv1v8ps
011800000616     C                   except    testdett
011900000613     C                   endif
012000000613     C*
012100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012300000613     C*
012400010202     C* Effettuo la chiamata al CLLE preposto
012500050128     C                   call(e)   'TITVVTC'
012600010202     C                   parm                    parccm
012700010202     C                   parm                    parmbr
012800010202     C                   parm      '2'           paropz
012900070730     C*
013000070730     C* Effettuo lancio TISI95 solo x chiusura
013100070730     C                   CLEAR                   TISI95DS
013200070730     C                   EVAL      I95TLA = 'C'
013300070730     C                   CALL      'TISI95R'
013400070730     C                   PARM                    TISI95DS
013500010202     C*
013600100310     C                   if        %open(titv1v8ps)
013700000616     C                   except    findett
013800100310     C                   close     titv1v8ps
013900000613     C                   endif
014000000616     C*
014100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014200011113     C                   Z-ADD     150           LENGH            15 5
014300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014400000616     C                   PARM                    CMD(3)
014500000616     C                   PARM                    LENGH
014600000616     C*
014700000801     C
014800010201     C                   seton                                        LR
014900000613
015000100310
015100100310
015200000613     C*--------------------------------------------------------
015300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015400000613     C*--------------------------------------------------------
015500000621     C     STPR          BEGSR
015600000613     C*
015700100310     C                   if        not %open(titv1v8p)
015800100310     C                   open      titv1v8p
015900990915     C                   endif
016000990915     C*
016100990915     C                   except    riepilogo
016200990915     C*
016300100310     C                   if        %open(titv1v8p)
016400100310     C                   close     titv1v8p
016500990914     C                   endif
016600990910     C*
016700000613     C                   ENDSR
016800000613     C***
016900990908
017000000801
017100910830     C*--------------------------------------------------------
017200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017300910830     C*--------------------------------------------------------
017400070530     C     RWFILE        BEGSR
017500990910     C*
017600990914     C                   if        not %open(tivin00r)
017700990908     C                   open      tivin00r
017800990914     C                   endif
017900021113     C                   if        not %open(fivabwwr)
018000021113     C                   open      fivabwwr
018100990914     C                   endif
018200070530     C*
018300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018400020305     C                   exsr      prevat
018500010201     C*
018600010202     C                   if        chkcall = '0'
018700010202     C*
018800021113     C                   if        not %open(fivatwwr)
018900021113     C                   open      fivatwwr
019000010201     C                   endif
019100080117     C*
019200080117     C                   EXSR      INZVAR
019300080117     C                   EXSR      DEFCAM
019400990910     C*
019500010201     C                   clear                   §CTROKVB          5 0
019600020305     C                   clear                   §CTROKVT          5 0
019700000801     C                   clear                   §CTRMO            5 0
019800000801     C                   clear                   §CTRNO            5 0
019900990910     C*
020000020725     C/EXEC SQL
020100020725     C+ declare C1 cursor for select
020200100310     C+ vindta, vinflg, substr(vindta, 8, 10) as sped, rrn(tivin00r)
020300060223     C+ from tivin00r
020400060223     C+ order by sped
020500020725     C+ for read only
020600020725     C/END-EXEC
020700020725     C
020800020725     C/EXEC SQL
020900020725     C+ open C1
021000020725     C/END-EXEC
021100020725     C
021200020725     C/EXEC SQL
021300070530     C+ Fetch C1 into :INPUT_DS
021400020725     C/END-EXEC
021500020725     C*
021600020725     C                   dow       sqlcod = *zeros
021700990913     C*
021800100310     C                   if        vindta > *blanks
021900000613     C                   add       1             rrnum
022000000801     C*
022100020725     C                   if        vinflg = *blanks
022200020725     C                             or vinflg = '0'
022300020725     C                             or vinflg = '2'
022400000801     C*
022500020725     C                   clear                   x_vinmsg
022600020725     C                   eval      x_vinflg = '1'
022700100310     C*
022800100310     C* Elaboro solo i record aventi i primi 3 byte a *blanks
022900100310     C                   if        %subst(vindta:1:3)        = *blanks AND
023000100310     C                             %trim(%subst(vindta:309:2)) = 'IT'
023100010305     C*
023200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
023300100310     C                   EVAL      curspe=%trim(%subst(vindta:8:10))
023400010305     C*
023500071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023600080923     C                   if        *in50 = *off
023700091223     C                   exsr      repNSP
023800071003     C                   exsr      impvab
023900071003     C                   exsr      wrivab
024000100127     C                   exsr      wrivat_a                                     => carico VAT
024100071003     C                   exsr      wrivat_b                                     => carico VAT
024200071003     C                   else
024300080923     C*
024400071009     C                   if        wDISK = 'D'
024500091223     C                   exsr      repNSP
024600071009     C                   exsr      impvab
024700071009     C                   exsr      wrivab
024800100127     C                   exsr      wrivat_a                                     => carico VAT
024900071009     C                   exsr      wrivat_b                                     => carico VAT
025000071009     C                   exsr      wrivat_e                                     => carico VAT
025100071009     C                   else
025200071009     C*
025300010305     C                   if        depspe = *blanks                             => 1° giro
025400010305     C                   eval      depspe = curspe                              => memorizz. spediz
025500080117     C                   clear                   tivinds
025600091223     C                   exsr      repNSP
025700020305     C                   exsr      impvab
025800100127     C                   exsr      wrivat_a                                     => carico VAT
025900071003     C                   exsr      wrivat_b                                     => carico VAT
026000071003     C   50              exsr      wrivat_e                                     => carico VAT
026100010305     C                   else
026200020725     C                   if        depspe <> curspe                             => rottura di spediz
026300010305     C                   eval      depspe = curspe                              => memorizz. spediz
026400070928     C                   exsr      wrivab
026500080117     C                   clear                   tivinds
026600091223     C                   exsr      repNSP
026700020305     C                   exsr      impvab
026800100127     C                   exsr      wrivat_a                                     => carico VAT
026900071003     C                   exsr      wrivat_b                                     => carico VAT
027000071003     C   50              exsr      wrivat_e                                     => carico VAT
027100020305     C                   else                                                   => x stessa spediz
027200100401     C*
027300100401     C* Se nn richiesta esclusione spedizioni "duplicate"
027400100401     C                   if        wDUPREC <> 'N'
027500020305     C                   exsr      impvab
027600100127     C                   exsr      wrivat_a                                     => carico VAT
027700071003     C                   exsr      wrivat_b                                     => carico VAT
027800071003     C   50              exsr      wrivat_e                                     => carico VAT
027900100401     C                   endif
028000010305     C                   endif
028100010305     C                   endif
028200010305     C                   endif
028300071003     C                   endif
028400071009     C                   endif
028500000905     C*
028600000905     C                   else
028700020725     C                   eval      x_vinflg = '1'
028800000905     C                   endif
028900100310     C*
029000100310     C                   endif
029100000905     C*
029200020725     C     VINRRN        chain     tivin000
029300020725     C                   if        %found(tivin00r)
029400020725     C                   eval      y_vinflg = x_vinflg
029500020725     C                   eval      y_vinmsg = x_vinmsg
029600020725     C                   update    tivin000
029700020725     C                   endif
029800020725     C*
029900020725     C/EXEC SQL
030000070530     C+ Fetch C1 into :INPUT_DS
030100020725     C/END-EXEC
030200020725     C*
030300020725     C                   enddo
030400020725     C*
030500020725     C/EXEC SQL
030600020725     C+ close C1
030700020725     C/END-EXEC
030800000905     C*
030900020305     C* Scarico i VAB rimasti "in sospeso"
031000071009     C                   if        wDISK = 'C'
031100071009     C                   exsr      wrivab
031200071009     C                   endif
031300010202     C*
031400010202     C                   endif
031500990910
031600990910     C* Se non ci sono record con errori ...
031700000710     C                   if        §ctrno = 0
031800990910     C* ... restituisco esito OK.
031900990921     C                   eval      wrkesito = '0'
032000990910     C                   else
032100010201     C                   if        §ctrokvb > 0
032200990921     C                   eval      wrkesito = '1'
032300000710     C                   else
032400000710     C                   eval      wrkesito = '2'
032500990910     C                   endif
032600000710     C                   endif
032700990910     C*
032800990914     C                   if        %open(tivin00r)
032900990908     C                   close     tivin00r
033000990914     C                   endif
033100021113     C                   if        %open(fivabwwr)
033200021113     C                   close     fivabwwr
033300990914     C                   endif
033400021113     C                   if        %open(fivatwwr)
033500021113     C                   close     fivatwwr
033600010201     C                   endif
033700990910     C*
033800010201     C                   if        §ctrokvb > 0
033900000724     C                             and vlrpoi <> *zeros
034000010202     C                   exsr      invio
034100990920     C                   endif
034200990920     C*
034300910830     C                   ENDSR
034400000613     C***
034500010305
034600010305     C*----------------------------------------------------*
034700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
034800010305     C*----------------------------------------------------*
034900020305     C     WRIVAB        BEGSR
035000070730     C*
035100071003     C* Considerazioni finali
035200071003     C                   if        VABRMA = *blanks
035300071003     C                   movel(P)  VABRMN        VABRMA
035400071003     C                   endif
035500071003     C*
035600021113     C                   write     fivab000                                     => scarico il VAB
035700010305     C*
035800010305     C                   ENDSR
035900990920
036000000801     C*----------------------------------------------------*
036100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036200000801     C*----------------------------------------------------*
036300010201     C     INZVAR        BEGSR
036400000801     C*
036500010201     C                   Z-ADD     *zeros        Num5_0
036600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036700020725     C                   MOVEL     '0'           FlgCAS            1
036800000801     C*
036900000801     C                   ENDSR
037000000801     C*----------------------------------------------------*
037100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
037200000801     C*----------------------------------------------------*
037300020904     C     DEFCAM        BEGSR
037400080923     C*
037500090310     C                   SETOFF                                       50
037600000801     C*
037700021113     C                   CLEAR                   FIVAB000
037800021113     C                   CLEAR                   FIVAT000
037900070730     C* Imposto i valori di default...
038000100310     C                   EVAL      VABCCM = 1110890
038100100310     C                   EVAL      VATCCM = 1110890
038200100310     C                   EVAL      VABLNP = 111
038300100310     C                   EVAL      VATLNP = 111
038400100309     C                   EVAL      VABCTR = 000
038500070730     C                   EVAL      VABCBO = '1'
038600100309     C                   EVAL      VABVCA = 'EUR'
038700070222     C* ... e poi verifico se sono stati passati come parametri
038800070222     C                   IF        vlrppt > *blanks
038900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
039000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039100070222     C                   EXSR      CHKNUM
039200070222     C                   IF        PiInt=*on
039300070222     C                   Z-ADD     PiVal         VABCCM
039400070222     C                   Z-ADD     PiVal         VATCCM
039500070222     C                   ENDIF
039600070222     C                   ENDIF
039700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
039800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039900070222     C                   EXSR      CHKNUM
040000070222     C                   IF        PiInt=*on
040100070222     C                   Z-ADD     PiVal         VABLNP
040200070222     C                   Z-ADD     PiVal         VATLNP
040300070222     C                   ENDIF
040400070222     C                   ENDIF
040500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
040600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040700070222     C                   EXSR      CHKNUM
040800070222     C                   IF        PiInt=*on
040900070222     C                   Z-ADD     PiVal         VABCTR
041000070222     C                   ENDIF
041100070928     C                   ENDIF
041200071009     C                   MOVEL     *blanks       wDISK             1
041300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
041400071009     C                   IF        wDISK <> *blanks
041500070928     C                   SETON                                        50
041600070222     C                   ENDIF
041700100401     C                   MOVEL     *blanks       wDUPREC           1
041800100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
041900070222     C                   ENDIF
042000071009     C*
042100071009     C   50              EVAL      VABCTM = '7Q'
042200000801     C*
042300000801     C                   ENDSR
042400091223     C*----------------------------------------------------*
042500091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
042600091223     C*----------------------------------------------------*
042700091223     C     REPNSP        BEGSR
042800091223     C*
042900091223     C                   EXSR      INZVAR
043000091223     C                   EXSR      DEFCAM
043100091223     C*
043200091223     C* NSP => Stacco un numeratore da AZNUM
043300091223     C                   clear                   TRUL33DS
043400091223     C                   eval      I33OPE = *zeros
043500091223     C                   eval      I33CNU = 302
043600091223     C                   eval      I33NUM = 1
043700091223     C                   movel     TRUL33DS      KPJBU
043800091223     C                   call      'TRUL33R'
043900091223     C                   parm                    KPJBA
044000091223     C                   movel     KPJBU         TRUL33DS
044100091223     C                   if        O33ERR = *zeros
044200091223     C                   z-add     O33NRF        VABNSP
044300091223     C                   z-add     O33NRF        VATNSP
044400091223     C                   else
044500091223     C                   SETON                                        31
044600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044700091223     C                             + ' ' + 'VABNSP VATNSP'
044800091223     C                   endif
044900091223     C*
045000091223     C                   ENDSR
045100000801     C*----------------------------------------------------*
045200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
045300000801     C*----------------------------------------------------*
045400010201     C     IMPVAB        BEGSR
045500070530     C*
045600070530     C                   SETOFF                                       3132
045700070928     C*
045800070928     C                   MOVE(P)   vlrpoi        VABFGS
045900070928     C                   MOVE(P)   vlrpoi        VATFGS
046000070928     C*
046100070928     C                   MOVEL     datcor        VABAAS
046200070928     C                   MOVEL     datcor        VATAAS
046300070928     C                   MOVE      datcor        VABMGS
046400100310     C                   EVAL      VABRSD=%trim(%subst(vindta:89:35))
046500070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046600070928     C     '@':'A'       XLATE     VABRSD        VABRSD
046700070928     C* ==
046800100310     C                   EVAL      VABRD2=%trim(%subst(vindta:127:35))
046900100310     C                   EVAL      VABIND=%trim(%subst(vindta:165:35))
047000100310     C                   EVAL      VABNOT=%trim(%subst(vindta:203:35))
047100100310     C                   EVAL      VABNT2=%trim(%subst(vindta:353:30))
047200100310     C                   EVAL      VABLOD=%trim(%subst(vindta:255:35))
047300100310     C                   EVAL      VABPRD=%trim(%subst(vindta:293:2))
047400100310     C                   EVAL      VABRMA=%trim(%subst(vindta:8:10))
047500100310     C* TC1/TC2
047600100310     C                   IF        %scan('APP':%subst(vindta:337:13)) > *zeros
047700100310     C                   EVAL      VABTC1 = 'A'
047800100310     C                   ENDIF
047900100310     C                   IF        %scan('PIA':%subst(vindta:337:13)) > *zeros
048000100310     C                   EVAL      VABTC2 = 'P'
048100100310     C                   ENDIF
048200100310     C
048300100310     C
048400100309     C* NCL
048500100310     C                   EVAL      PiStr=%trim(%subst(vindta:38:3))
048600100309     C                   EXSR      CHKNUM
048700100309     C                   IF        PiInt=*on
048800100309     C                   Z-ADD     PiVal         VABNCL
048900100309     C                   ELSE
049000100309     C                   SETON                                        32
049100100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200100309     C                             + ' ' + 'VABNCL'
049300100309     C                   ENDIF
049400100309     C* PKB
049500100310     C                   IF        %trim(%subst(vindta:78:2)) = 'KG'
049600100310     C                   EVAL      PiStr=%trim(%subst(vindta:66:9))
049700100309     C                   EXSR      CHKNUM
049800100309     C                   IF        PiNum=*on
049900100309     C                   Z-ADD     PiVal         VABPKB
050000100309     C                   ELSE
050100100309     C                   SETON                                        32
050200100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300100309     C                             + ' ' + 'VABPKB'
050400100309     C                   ENDIF
050500100310     C                   ENDIF
050600071003     C* CAD
050700100310     C                   EVAL      PiStr=%trim(%subst(vindta:241:12))
050800070928     C                   EXSR      CHKNUM
050900070928     C                   IF        PiInt=*on
051000070928     C                   Z-ADD     PiVal         Num5_0
051100070928     C                   MOVEL     Num5_0        VABCAD
051200070928     C                   ELSE
051300070928     C                   SETON                                        32
051400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051500070928     C                             + ' ' + 'VABCAD'
051600070928     C                   ENDIF
051700080923     C* RMN
051800100310     C                   EVAL      PiStr=%trim(%subst(vindta:8:10))
051900080923     C                   EXSR      CHKNUM
052000080923     C                   IF        PiInt=*on
052100080923     C                   Z-ADD     PiVal         VABRMN
052200100310     C                   Z-ADD     PiVal         wWHO              7 0
052300080923     C                   ELSE
052400080923     C                   SETON                                        32
052500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052600080923     C                             + ' ' + 'VABRMN'
052700080923     C                   ENDIF
052800100310     C*
052900100310     C* Se provincia nn valorizzata la reperisco
053000100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
053100100310     C                   IF        VABNZD = *blanks AND
053200100310     C                             VABPRD = *blanks
053300100310     C                   CLEAR                   TISI95DS
053400100310     C                   EVAL      I95TCN = '3'
053500100310     C                   Z-ADD     datcor        I95DAT
053600100310     C                   EVAL      I95NAR = VABNZD
053700100310     C                   EVAL      I95CAP = VABCAD
053800100310     C                   EVAL      I95LOC = VABLOD
053900100310     C                   CALL      'TISI95R'
054000100310     C                   PARM                    TISI95DS
054100100310     C                   EVAL      VABPRD = O95PRV
054200100310     C                   ENDIF
054300070730     C*
054400070730     C* Considerazioni finali su CBO/CAS
054500070730     C                   IF        FlgCAS = '1'
054600070730     C                   IF        VABCBO = '1'
054700070730     C                   EVAL      VABCBO = '4'
054800070730     C                   ENDIF
054900070730     C                   IF        VABCBO = '2'
055000070730     C                   EVAL      VABCBO = '6'
055100070730     C                   ENDIF
055200070730     C                   ENDIF
055300020305     C*
055400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055500011113     C                   EXSR      CHKIMPDIV
055600010202     C*
055700000801     C* Ebbene...
055800000801     C                   ADD       1             §CTRMO
055900070530     C                   IF        *in31 <> *zeros OR
056000070530     C                             *in32 <> *zeros
056100000801     C                   ADD       1             §CTRNO
056200000801     C                   EVAL      recko = vindta
056300000801     C                   EXCEPT    rigadett
056400020725     C                   EVAL      x_vinflg = '2'
056500000801     C                   ELSE
056600010201     C                   ADD       1             §CTROKVB
056700000801     C                   ENDIF
056800000801     C*
056900000801     C                   ENDSR
057000100127     C*----------------------------------------------------*
057100100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057200100127     C*----------------------------------------------------*
057300100127     C     WRIVAT_A      BEGSR
057400100127     C*
057500100127     C* Valorizzo l buffer di scrittura del FIVAT
057600100127     C* - TIPO RECORD "A"
057700100310     C                   eval      VATTRC = 'A'
057800100310     C                   eval      VATNOT = %trim(%subst(vindta:386:25))
057900100310     C                   exsr      wrivat
058000100127     C*
058100100127     C                   ENDSR
058200010201     C*----------------------------------------------------*
058300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
058400010201     C*----------------------------------------------------*
058500071003     C     WRIVAT_B      BEGSR
058600010201     C*
058700021113     C* Valorizzo l buffer di scrittura del FIVAT
058800070928     C* - TIPO RECORD "B"
058900100310     C                   eval      VATTRC = 'B'
059000100310     C                   eval      VATNOT = %trim(%subst(vindta:414:19))
059100100310     C                   exsr      wrivat
059200010201     C*
059300010201     C                   ENDSR
059400071003     C*----------------------------------------------------*
059500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059600071003     C*----------------------------------------------------*
059700071003     C     WRIVAT_E      BEGSR
059800071003     C*
059900071003     C* Valorizzo l buffer di scrittura del FIVAT
060000071003     C* - TIPO RECORD "E"
060100100310     C                   eval      VATTRC = 'E'
060200100310     C*
060300100310     C* X ogni collo indicato in testata
060400100310     C                   z-add     1             wColli            5 0
060500100310     C                   dow       wColli <= VABNCL
060600100310     C*
060700100310     C* Valorizzo il buffer di scrittura del FIVAT
060800100310     C                   move(p)   wColli        wColliAlfa        3
060900100318     C                   eval      VATNOT = %trim(%editc(wWHO:'Z')+'BIBBY'+
061000100318     C                                      wColliAlfa)
061100100310     C                   exsr      wrivat                                       => scarico VAT
061200100310     C*
061300100310     C                   add       1             wColli
061400100310     C                   enddo
061500071003     C*
061600071003     C                   ENDSR
061700100127     C*----------------------------------------------------*
061800100127     C*  SCARICO BUFFER FIVAT
061900100127     C*----------------------------------------------------*
062000100127     C     WRIVAT        BEGSR
062100100127     C*
062200100127     C                   if        vatNOT <> *blanks
062300100127     C                   ADD       1             §CTROKVT
062400100127     C                   write     FIVAT000
062500100127     C                   endif
062600100127     C*
062700100127     C                   ENDSR
062800020904
062900020904
063000010202     C*----------------------------------------------------*
063100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
063200010202     C*----------------------------------------------------*
063300020305     C     PREVAT        BEGSR
063400010202     C*
063500021113     C* Compongo il nome del membro da dare al FIVATWWR
063600010202     C                   eval      parmbr = vlrhdl
063700010202     C                   movel     'M'           parmbr
063800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
063900010202     C                   eval      paropz = '1'
064000010202     C* Effettuo la chiamata al CLLE preposto
064100050128     C                   call(e)   'TITVVTC'
064200010202     C                   parm                    parccm
064300010202     C                   parm                    parmbr
064400010202     C                   parm                    paropz
064500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064600010202     C                   if        %error
064700010202     C                   movel     '1'           chkcall
064800010202     C                   else
064900010202     C                   movel     '0'           chkcall
065000010202     C                   endif
065100010202     C*
065200010202     C                   ENDSR
065300000801     C*----------------------------------------------------*
065400000801     C*  CONTROLLO NUMERICITA' CAMPI
065500000801     C*----------------------------------------------------*
065600000801     C     CHKNUM        BEGSR
065700081113     C*
065800081113     C                   IF        PiDecChr = *blanks
065900091223     C                   EVAL      PiDecChr = ','
066000081113     C                   ENDIF
066100091223     C*
066200081113     C                   callp(e)  UBISNUM_Check(PiStr
066300081113     C                                          :PiDecChr
066400081113     C                                          :PiVal
066500081113     C                                          :PiNum
066600081113     C                                          :PiInt)
066700081113     C*
066800000801     C                   IF        %error
066900000801     C                   EVAL      PiInt=*off
067000000801     C                   ENDIF
067100000801     C*
067200000801     C                   ENDSR
067300000801     C***
067400000801
067500011113
067600011113     C*----------------------------------------------------*
067700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
067800011113     C*----------------------------------------------------*
067900011113     C     CHKIMPDIV     BEGSR
068000011113     C*
068100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
068200011113     C                   Z-ADD     *zeros        wrkDec            9 9
068300011113     C*
068400011113     C* Come prima cosa effettuo considerazioni sulla divisa
068500011113     C                   IF        vabIAS > *zeros
068600011113     C                   IF        vabVAS <> 'EUR'
068700011113     C                   EVAL      vabVAS =  'ITL'
068800011113     C                   ENDIF
068900011113     C                   ENDIF
069000011113     C*
069100011113     C                   IF        vabCAS > *zeros
069200011113     C                   IF        vabVCA <> 'EUR'
069300011113     C                   EVAL      vabVCA =  'ITL'
069400011113     C                   ENDIF
069500011113     C                   ENDIF
069600011113     C*
069700011113     C                   IF        vabVMD > *zeros
069800020305     C                   IF        vabVAD <> 'EUR'
069900011113     C                   EVAL      vabVAD =  'ITL'
070000011113     C                   ENDIF
070100011113     C                   ENDIF
070200011113     C*
070300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
070400011113     C                   Z-ADD     vabIAS        wrkDec
070500011113     C                   IF        wrkDec > *zeros
070600011113     C                   IF        vabVAS = 'ITL'
070700011113     C                   EVAL      vabIAS = *zeros
070800011113     C                   ENDIF
070900011113     C                   ENDIF
071000011113     C*
071100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
071200011113     C                   Z-ADD     vabCAS        wrkDec
071300011113     C                   IF        wrkDec > *zeros
071400011113     C                   IF        vabVCA = 'ITL'
071500011113     C                   EVAL      vabCAS = *zeros
071600011113     C                   ENDIF
071700011113     C                   ENDIF
071800011113     C*
071900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
072000011113     C                   Z-ADD     vabVMD        wrkDec
072100011113     C                   IF        wrkDec > *zeros
072200011113     C                   IF        vabVAD = 'ITL'
072300011113     C                   EVAL      vabVMD = *zeros
072400011113     C                   ENDIF
072500011113     C                   ENDIF
072600011113     C*
072700011113     C                   ENDSR
072800011113     C***
072900011113
073000011113
073100000801
073200000801
073300990920      /TITLE Invio dei dati al punto operativo.
073400010202     C     invio         BEGSR
073500990920     C*
073600021113     C* 1° invio FIVAT
073700010201     C                   reset                   dscmz
073800010201     C                   move      vlrpoi        cmzdst
073900021113     C                   eval      cmzfld = 'FIVATWWR'
074000010201     C                   eval      cmzmbd = vlrhdl
074100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074200021009     C***                if        prmfir = *blanks
074300021113     C                   eval      cmzfla = 'FIVAT00F'
074400021113     C                   eval      cmzmba = 'FIVAT00F'
074500021009     C***                else
074600021009     C***                eval      cmzfla = prmfir
074700021009     C***                eval      cmzmba = prmfir
074800021009     C***                endif
074900010201     C                   eval      cmznrr = *zeros
075000020305     C                   move      §ctrokvt      cmznrr
075100021018     C                   eval      cmzlba = vlrfl1
075200010201     C                   call(e)   'TIS711C'
075300010201     C                   parm                    dscmz
075400010201     C                   parm      *blanks       esito
075500010205     C                   if        %error
075600010205     C                             or cmzerr = '1'
075700010205     C                             or esito  = '1'
075800010205     C                   eval      wrkesito = '3'
075900010205     C                   else
076000010201     C*
076100021113     C* 2° invio FIVAB
076200010201     C                   reset                   dscmz
076300010201     C                   move      vlrpoi        cmzdst
076400010201     C                   eval      cmzfld = vlrfou
076500010201     C                   eval      cmzmbd = vlrhdl
076600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076700021009     C***                if        prmfir = *blanks
076800021113     C                   eval      cmzfla = 'FIVAB00F'
076900021113     C                   eval      cmzmba = 'FIVAB00F'
077000021009     C***                else
077100021009     C***                eval      cmzfla = prmfir
077200021009     C***                eval      cmzmba = prmfir
077300021009     C***                endif
077400010201     C                   eval      cmznrr = *zeros
077500010201     C                   move      §ctrokvb      cmznrr
077600021018     C                   eval      cmzlba = vlrfl1
077700010201     C                   call(e)   'TIS711C'
077800010201     C                   parm                    dscmz
077900010201     C                   parm      *blanks       esito
078000010201     C                   if        %error
078100010201     C                             or cmzerr = '1'
078200010201     C                             or esito  = '1'
078300010201     C                   eval      wrkesito = '3'
078400010201     C                   endif
078500010205     C                   endif
078600990920     C*
078700000613     C                   ENDSR
078800000613     C***
078900070411
079000070411     C     *pssr         BEGSR
079100070411     C*
079200070411     C                   if        %open(tivin00r)
079300070411     C                   close     tivin00r
079400070411     C                   endif
079500070411     C                   if        %open(fivabwwr)
079600070411     C                   close     fivabwwr
079700070411     C                   endif
079800070411     C                   if        %open(fivatwwr)
079900070411     C                   close     fivatwwr
080000070411     C                   endif
080100070411     C*
080200070411     C* Effettuo la chiamata al CLLE preposto
080300070411     C                   call(e)   'TITVVTC'
080400070411     C                   parm                    parccm
080500070411     C                   parm                    parmbr
080600070411     C                   parm      '2'           paropz
080700070411     C*
080800070411     C                   eval      wrkesito = '2'
080900070411     C*
081000070411     C                   seton                                        LR
081100070411     C*
081200070411     C                   ENDSR     '*CANCL'
081300070411     C***
081400070411
081500990910
081600000613     C     *inzsr        BEGSR
081700990910     C*
081800990910     C     *entry        plist
081900990920     C                   parm                    tivlrds
082000990921     C                   parm      wrkesito      esito
082100000724     C                   parm                    prmlit
082200000710     C                   parm                    prmfir
082300000613     C*
082400000830     C* CALCOLA LA DATA CORRENTE
082500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082600091223     C                   eval      datcor = %dec(%date() : *ISO)
082700000830     C*
082800000613     C                   ENDSR
082900000613     C***
083000990908
083100100310     Otitv1v8p  E            riepilogo         2
083200990915     O                                              'Upload via Internet'
083300990915     O                                           +1 'Traduzione TIVIN00R -'
083400021113     O                                           +1 'FIVABWWR/FIVATWWR'
083500010201     O                                           +1 'Report testate bolle'
083600990915     O          E            riepilogo   2
083700990915     O                       wrkdata
083800990915     O                       wrkora              +1
083900990915     O                       procname            +1
084000990915     O          E            riepilogo   2
084100990915     O                                              'Cliente..................:'
084200990915     O                       VABCCM        z     +1
084300990915     O          E            riepilogo   2
084400990920     O                                              'Riferimento Strategi.....:'
084500990920     O                       vlrhdl              +1
084600990915     O          E            riepilogo   2
084700990915     O                                              'Giusti...................:'
084800010201     O                       §CTROKVB      2   +  1
084900990915     O          E            riepilogo   2
085000990915     O                                              'Sbagliati e corretti.....:'
085100971022     O                       §CTRMO        2   +  1
085200990915     O          E            riepilogo   2
085300990915     O                                              'Sbagliati e scartati.....:'
085400971022     O                       §CTRNO        2   +  1
085500000613
085600100310     Otitv1v8ps E            testdett          2
085700000613     O                                              'Upload via Internet'
085800000613     O                                           +1 'Traduzione TIVIN00R -'
085900021113     O                                           +1 'FIVABWWR/FIVATWWR'
086000010201     O                                           +1 'Report testate bolle'
086100000616     O          E            testdett    3
086200000613     O                                           +2 'N° rec'
086300000613     O                                           +3 'Anteprima contenuto'
086400000616     O          E            rigadett    2
086500000613     O                       rrnum               +2
086600000621     O                       recko               +3
086700000616     O          E            findett     2
086800000613     O                       wrkdata
086900000613     O                       wrkora              +1
087000000613     O                       procname            +1
087100000616     O          E            findett     2
087200000613     O                                              'Cliente..................:'
087300000613     O                       VABCCM        z     +1
087400000616     O          E            findett     2
087500000613     O                                              'Riferimento Strategi.....:'
087600000613     O                       vlrhdl              +1
087700000616     O          E            findett     2
087800000613     O                                              'Giusti...................:'
087900010201     O                       §CTROKVB      2   +  1
088000000616     O          E            findett     2
088100000613     O                                              'Sbagliati e corretti.....:'
088200000613     O                       §CTRMO        2   +  1
088300000616     O          E            findett     2
088400000613     O                                              'Sbagliati e scartati.....:'
088500000613     O                       §CTRNO        2   +  1
088600000512** CMD - COMANDI CL
088700100310OVRPRTF FILE(TITV1V8P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
088800100310OVRPRTF FILE(TITV1V8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
088900100310DLTOVR FILE(TITV1V8P TITV1V8PS) LVL(*)
089000000512
089100000512
