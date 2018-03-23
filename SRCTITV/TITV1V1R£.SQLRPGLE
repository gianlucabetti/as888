000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100222     Ftitv1v1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100222     Ftitv1v1ps O    f  198        PRINTER usropn
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
003600100127     D depspe          s             80    INZ(*blanks)
003700100127     D curspe          s             80    INZ(*blanks)
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
005800091223     D trul33ds      e ds                  inz
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
011600100222     C                   if        not %open(titv1v1ps)
011700100222     C                   open      titv1v1ps
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
013600100222     C                   if        %open(titv1v1ps)
013700000616     C                   except    findett
013800100222     C                   close     titv1v1ps
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
015000000613     C*--------------------------------------------------------
015100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015200000613     C*--------------------------------------------------------
015300000621     C     STPR          BEGSR
015400000613     C*
015500100222     C                   if        not %open(titv1v1p)
015600100222     C                   open      titv1v1p
015700990915     C                   endif
015800990915     C*
015900990915     C                   except    riepilogo
016000990915     C*
016100100222     C                   if        %open(titv1v1p)
016200100222     C                   close     titv1v1p
016300990914     C                   endif
016400990910     C*
016500000613     C                   ENDSR
016600000613     C***
016700990908
016800000801
016900910830     C*--------------------------------------------------------
017000070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017100910830     C*--------------------------------------------------------
017200070530     C     RWFILE        BEGSR
017300990910     C*
017400990914     C                   if        not %open(tivin00r)
017500990908     C                   open      tivin00r
017600990914     C                   endif
017700021113     C                   if        not %open(fivabwwr)
017800021113     C                   open      fivabwwr
017900990914     C                   endif
018000070530     C*
018100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018200020305     C                   exsr      prevat
018300010201     C*
018400010202     C                   if        chkcall = '0'
018500010202     C*
018600021113     C                   if        not %open(fivatwwr)
018700021113     C                   open      fivatwwr
018800010201     C                   endif
018900080117     C*
019000080117     C                   EXSR      INZVAR
019100080117     C                   EXSR      DEFCAM
019200990910     C*
019300010201     C                   clear                   §CTROKVB          5 0
019400020305     C                   clear                   §CTROKVT          5 0
019500000801     C                   clear                   §CTRMO            5 0
019600000801     C                   clear                   §CTRNO            5 0
019700990910     C*
019800020725     C/EXEC SQL
019900020725     C+ declare C1 cursor for select
020000100222     C+ vindta, vinflg, substr(vindta, 17, 12) as sped, rrn(tivin00r)
020100060223     C+ from tivin00r
020200060223     C+ order by sped
020300020725     C+ for read only
020400020725     C/END-EXEC
020500020725     C
020600020725     C/EXEC SQL
020700020725     C+ open C1
020800020725     C/END-EXEC
020900020725     C
021000020725     C/EXEC SQL
021100070530     C+ Fetch C1 into :INPUT_DS
021200020725     C/END-EXEC
021300020725     C*
021400020725     C                   dow       sqlcod = *zeros
021500990913     C*
021600020725     C                   if        vindta > *blanks
021700000613     C                   add       1             rrnum
021800000801     C*
021900020725     C                   if        vinflg = *blanks
022000020725     C                             or vinflg = '0'
022100020725     C                             or vinflg = '2'
022200000801     C*
022300020725     C                   clear                   x_vinmsg
022400020725     C                   eval      x_vinflg = '1'
022500010305     C*
022600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
022700100222     C                   EVAL      curspe=%trim(%subst(vindta:17:12))
022800010305     C*
022900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023000080923     C                   if        *in50 = *off
023100091223     C                   exsr      repNSP
023200071003     C                   exsr      impvab
023300071003     C                   exsr      wrivab
023400100127     C                   exsr      wrivat_a                                     => carico VAT
023500071003     C                   exsr      wrivat_b                                     => carico VAT
023600071003     C                   else
023700080923     C*
023800071009     C                   if        wDISK = 'D'
023900091223     C                   exsr      repNSP
024000071009     C                   exsr      impvab
024100071009     C                   exsr      wrivab
024200100127     C                   exsr      wrivat_a                                     => carico VAT
024300071009     C                   exsr      wrivat_b                                     => carico VAT
024400071009     C                   exsr      wrivat_e                                     => carico VAT
024500071009     C                   else
024600071009     C*
024700010305     C                   if        depspe = *blanks                             => 1° giro
024800010305     C                   eval      depspe = curspe                              => memorizz. spediz
024900080117     C                   clear                   tivinds
025000091223     C                   exsr      repNSP
025100020305     C                   exsr      impvab
025200100127     C                   exsr      wrivat_a                                     => carico VAT
025300071003     C                   exsr      wrivat_b                                     => carico VAT
025400071003     C   50              exsr      wrivat_e                                     => carico VAT
025500010305     C                   else
025600020725     C                   if        depspe <> curspe                             => rottura di spediz
025700010305     C                   eval      depspe = curspe                              => memorizz. spediz
025800070928     C                   exsr      wrivab
025900080117     C                   clear                   tivinds
026000091223     C                   exsr      repNSP
026100020305     C                   exsr      impvab
026200100127     C                   exsr      wrivat_a                                     => carico VAT
026300071003     C                   exsr      wrivat_b                                     => carico VAT
026400071003     C   50              exsr      wrivat_e                                     => carico VAT
026500020305     C                   else                                                   => x stessa spediz
026600020305     C                   exsr      impvab
026700100127     C                   exsr      wrivat_a                                     => carico VAT
026800071003     C                   exsr      wrivat_b                                     => carico VAT
026900071003     C   50              exsr      wrivat_e                                     => carico VAT
027000010305     C                   endif
027100010305     C                   endif
027200010305     C                   endif
027300071003     C                   endif
027400071009     C                   endif
027500000905     C*
027600000905     C                   else
027700020725     C                   eval      x_vinflg = '1'
027800000905     C                   endif
027900000905     C*
028000020725     C     VINRRN        chain     tivin000
028100020725     C                   if        %found(tivin00r)
028200020725     C                   eval      y_vinflg = x_vinflg
028300020725     C                   eval      y_vinmsg = x_vinmsg
028400020725     C                   update    tivin000
028500020725     C                   endif
028600020725     C*
028700020725     C/EXEC SQL
028800070530     C+ Fetch C1 into :INPUT_DS
028900020725     C/END-EXEC
029000020725     C*
029100020725     C                   enddo
029200020725     C*
029300020725     C/EXEC SQL
029400020725     C+ close C1
029500020725     C/END-EXEC
029600000905     C*
029700020305     C* Scarico i VAB rimasti "in sospeso"
029800071009     C                   if        wDISK = 'C'
029900071009     C                   exsr      wrivab
030000071009     C                   endif
030100010202     C*
030200010202     C                   endif
030300990910
030400990910     C* Se non ci sono record con errori ...
030500000710     C                   if        §ctrno = 0
030600990910     C* ... restituisco esito OK.
030700990921     C                   eval      wrkesito = '0'
030800990910     C                   else
030900010201     C                   if        §ctrokvb > 0
031000990921     C                   eval      wrkesito = '1'
031100000710     C                   else
031200000710     C                   eval      wrkesito = '2'
031300990910     C                   endif
031400000710     C                   endif
031500990910     C*
031600990914     C                   if        %open(tivin00r)
031700990908     C                   close     tivin00r
031800990914     C                   endif
031900021113     C                   if        %open(fivabwwr)
032000021113     C                   close     fivabwwr
032100990914     C                   endif
032200021113     C                   if        %open(fivatwwr)
032300021113     C                   close     fivatwwr
032400010201     C                   endif
032500990910     C*
032600010201     C                   if        §ctrokvb > 0
032700000724     C                             and vlrpoi <> *zeros
032800010202     C                   exsr      invio
032900990920     C                   endif
033000990920     C*
033100910830     C                   ENDSR
033200000613     C***
033300010305
033400010305     C*----------------------------------------------------*
033500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
033600010305     C*----------------------------------------------------*
033700020305     C     WRIVAB        BEGSR
033800070730     C*
033900071003     C* Considerazioni finali
034000071003     C                   if        VABRMA = *blanks
034100071003     C                   movel(P)  VABRMN        VABRMA
034200071003     C                   endif
034300071003     C*
034400021113     C                   write     fivab000                                     => scarico il VAB
034500010305     C*
034600010305     C                   ENDSR
034700990920
034800000801     C*----------------------------------------------------*
034900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035000000801     C*----------------------------------------------------*
035100010201     C     INZVAR        BEGSR
035200000801     C*
035300010201     C                   Z-ADD     *zeros        Num5_0
035400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
035500020725     C                   MOVEL     '0'           FlgCAS            1
035600000801     C*
035700000801     C                   ENDSR
035800000801     C*----------------------------------------------------*
035900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
036000000801     C*----------------------------------------------------*
036100020904     C     DEFCAM        BEGSR
036200080923     C*
036300090310     C                   SETOFF                                       50
036400000801     C*
036500021113     C                   CLEAR                   FIVAB000
036600021113     C                   CLEAR                   FIVAT000
036700070730     C* Imposto i valori di default...
036800100222     C                   EVAL      VABCCM = 0895142
036900100222     C                   EVAL      VATCCM = 0895142
037000100222     C                   EVAL      VABLNP = 089
037100100222     C                   EVAL      VATLNP = 089
037200100222     C                   EVAL      VABCTR = 300
037300070730     C                   EVAL      VABCBO = '1'
037400100222     C                   EVAL      VABCTM = '7Q'
037500100222     C                   EVAL      VABTSP = 'D'
037600100222     C                   EVAL      VABNCL = 1
037700100222     C                   EVAL      VABPKB = 1
037800100222     C                   EVAL      VABNAS = 'VARIE'
037900100315     C                   EVAL      VABNOT ='RICHIESTA VARIAZIONE INDIRIZZO E'
038000100315     C                   EVAL      VABNT2 ='FERMO DEP AUTORIZZATO - NO GIACENZA'
038100070222     C* ... e poi verifico se sono stati passati come parametri
038200070222     C                   IF        vlrppt > *blanks
038300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
038400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
038500070222     C                   EXSR      CHKNUM
038600070222     C                   IF        PiInt=*on
038700070222     C                   Z-ADD     PiVal         VABCCM
038800070222     C                   Z-ADD     PiVal         VATCCM
038900070222     C                   ENDIF
039000070222     C                   ENDIF
039100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
039200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039300070222     C                   EXSR      CHKNUM
039400070222     C                   IF        PiInt=*on
039500070222     C                   Z-ADD     PiVal         VABLNP
039600070222     C                   Z-ADD     PiVal         VATLNP
039700070222     C                   ENDIF
039800070222     C                   ENDIF
039900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
040000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040100070222     C                   EXSR      CHKNUM
040200070222     C                   IF        PiInt=*on
040300070222     C                   Z-ADD     PiVal         VABCTR
040400070222     C                   ENDIF
040500070928     C                   ENDIF
040600071009     C                   MOVEL     *blanks       wDISK             1
040700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
040800071009     C                   IF        wDISK <> *blanks
040900070928     C                   SETON                                        50
041000070222     C                   ENDIF
041100070222     C                   ENDIF
041200071009     C*
041300071009     C   50              EVAL      VABCTM = '7Q'
041400000801     C*
041500000801     C                   ENDSR
041600091223     C*----------------------------------------------------*
041700091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041800091223     C*----------------------------------------------------*
041900091223     C     REPNSP        BEGSR
042000091223     C*
042100091223     C                   EXSR      INZVAR
042200091223     C                   EXSR      DEFCAM
042300091223     C*
042400091223     C* NSP => Stacco un numeratore da AZNUM
042500091223     C                   clear                   TRUL33DS
042600091223     C                   eval      I33OPE = *zeros
042700091223     C                   eval      I33CNU = 302
042800091223     C                   eval      I33NUM = 1
042900091223     C                   movel     TRUL33DS      KPJBU
043000091223     C                   call      'TRUL33R'
043100091223     C                   parm                    KPJBA
043200091223     C                   movel     KPJBU         TRUL33DS
043300091223     C                   if        O33ERR = *zeros
043400091223     C                   z-add     O33NRF        VABNSP
043500091223     C                   z-add     O33NRF        VATNSP
043600091223     C                   else
043700091223     C                   SETON                                        31
043800091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043900091223     C                             + ' ' + 'VABNSP VATNSP'
044000091223     C                   endif
044100091223     C*
044200091223     C                   ENDSR
044300000801     C*----------------------------------------------------*
044400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044500000801     C*----------------------------------------------------*
044600010201     C     IMPVAB        BEGSR
044700070530     C*
044800070530     C                   SETOFF                                       3132
044900070928     C*
045000070928     C                   MOVE(P)   vlrpoi        VABFGS
045100070928     C                   MOVE(P)   vlrpoi        VATFGS
045200070928     C*
045300070928     C                   MOVEL     datcor        VABAAS
045400070928     C                   MOVEL     datcor        VATAAS
045500070928     C                   MOVE      datcor        VABMGS
045600100222     C                   EVAL      VABRSD=%trim(%subst(vindta:29:30))
045700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
045900070928     C* ==
046000100222     C                   EVAL      VABIND=%trim(%subst(vindta:59:30))
046100100224     C                   MOVEL     *blanks       wLOD             35
046200100224     C                   EVAL      wLOD=%trim(%subst(vindta:94:30))
046300100224     C                   IF        %len(%trim(wLOD)) > 2
046400100224     C                   EVAL      VABLOD=%trim(%subst(%trim(wLOD):1:
046500100224     C                                    %len(%trim(wLOD))-3))
046600100224     C                   EVAL      VABPRD=%trim(%subst(%trim(wLOD):
046700100224     C                                    %len(%trim(wLOD))-2+1:2))
046800100222     C                   ELSE
046900100224     C                   EVAL      VABLOD=wLOD
047000100222     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
047100100222     C                   ENDIF
047200100222     C                   EVAL      VABRMA=%trim(%subst(vindta:17:12))
047300071003     C* CAD
047400100222     C                   EVAL      PiStr=%trim(%subst(vindta:89:5))
047500070928     C                   EXSR      CHKNUM
047600070928     C                   IF        PiInt=*on
047700070928     C                   Z-ADD     PiVal         Num5_0
047800070928     C                   MOVEL     Num5_0        VABCAD
047900070928     C                   ELSE
048000070928     C                   SETON                                        32
048100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048200070928     C                             + ' ' + 'VABCAD'
048300070928     C                   ENDIF
048400080923     C* RMN
048500100222     C                   EVAL      PiStr=%trim(%subst(vindta:17:12))
048600080923     C                   EXSR      CHKNUM
048700080923     C                   IF        PiInt=*on
048800080923     C                   Z-ADD     PiVal         VABRMN
048900080923     C                   ELSE
049000080923     C                   SETON                                        32
049100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200080923     C                             + ' ' + 'VABRMN'
049300080923     C                   ENDIF
049400070730     C*
049500070730     C* Considerazioni finali su CBO/CAS
049600070730     C                   IF        FlgCAS = '1'
049700070730     C                   IF        VABCBO = '1'
049800070730     C                   EVAL      VABCBO = '4'
049900070730     C                   ENDIF
050000070730     C                   IF        VABCBO = '2'
050100070730     C                   EVAL      VABCBO = '6'
050200070730     C                   ENDIF
050300070730     C                   ENDIF
050400020305     C*
050500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050600011113     C                   EXSR      CHKIMPDIV
050700010202     C*
050800000801     C* Ebbene...
050900000801     C                   ADD       1             §CTRMO
051000070530     C                   IF        *in31 <> *zeros OR
051100070530     C                             *in32 <> *zeros
051200000801     C                   ADD       1             §CTRNO
051300000801     C                   EVAL      recko = vindta
051400000801     C                   EXCEPT    rigadett
051500020725     C                   EVAL      x_vinflg = '2'
051600000801     C                   ELSE
051700010201     C                   ADD       1             §CTROKVB
051800000801     C                   ENDIF
051900000801     C*
052000000801     C                   ENDSR
052100100127     C*----------------------------------------------------*
052200100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
052300100127     C*----------------------------------------------------*
052400100127     C     WRIVAT_A      BEGSR
052500100127     C*
052600100127     C* Valorizzo l buffer di scrittura del FIVAT
052700100127     C* - TIPO RECORD "A"
052800100127     C***                eval      VATTRC = 'A'
052900100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
053000100127     C***                exsr      wrivat
053100100127     C*
053200100127     C                   ENDSR
053300010201     C*----------------------------------------------------*
053400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
053500010201     C*----------------------------------------------------*
053600071003     C     WRIVAT_B      BEGSR
053700010201     C*
053800021113     C* Valorizzo l buffer di scrittura del FIVAT
053900070928     C* - TIPO RECORD "B"
054000100127     C***                eval      VATTRC = 'B'
054100100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
054200100127     C***                exsr      wrivat
054300010201     C*
054400010201     C                   ENDSR
054500071003     C*----------------------------------------------------*
054600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054700071003     C*----------------------------------------------------*
054800071003     C     WRIVAT_E      BEGSR
054900071003     C*
055000071003     C* Valorizzo l buffer di scrittura del FIVAT
055100071003     C* - TIPO RECORD "E"
055200100222     C                   eval      VATTRC = 'E'
055300100222     C                   eval      VATNOT = %trim(%subst(vindta:17:12))
055400100222     C                   exsr      wrivat
055500071003     C*
055600071003     C                   ENDSR
055700100127     C*----------------------------------------------------*
055800100127     C*  SCARICO BUFFER FIVAT
055900100127     C*----------------------------------------------------*
056000100127     C     WRIVAT        BEGSR
056100100127     C*
056200100127     C                   if        vatNOT <> *blanks
056300100127     C                   ADD       1             §CTROKVT
056400100127     C                   write     FIVAT000
056500100127     C                   endif
056600100127     C*
056700100127     C                   ENDSR
056800020904
056900020904
057000010202     C*----------------------------------------------------*
057100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057200010202     C*----------------------------------------------------*
057300020305     C     PREVAT        BEGSR
057400010202     C*
057500021113     C* Compongo il nome del membro da dare al FIVATWWR
057600010202     C                   eval      parmbr = vlrhdl
057700010202     C                   movel     'M'           parmbr
057800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057900010202     C                   eval      paropz = '1'
058000010202     C* Effettuo la chiamata al CLLE preposto
058100050128     C                   call(e)   'TITVVTC'
058200010202     C                   parm                    parccm
058300010202     C                   parm                    parmbr
058400010202     C                   parm                    paropz
058500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058600010202     C                   if        %error
058700010202     C                   movel     '1'           chkcall
058800010202     C                   else
058900010202     C                   movel     '0'           chkcall
059000010202     C                   endif
059100010202     C*
059200010202     C                   ENDSR
059300000801     C*----------------------------------------------------*
059400000801     C*  CONTROLLO NUMERICITA' CAMPI
059500000801     C*----------------------------------------------------*
059600000801     C     CHKNUM        BEGSR
059700081113     C*
059800081113     C                   IF        PiDecChr = *blanks
059900091223     C                   EVAL      PiDecChr = ','
060000081113     C                   ENDIF
060100091223     C*
060200081113     C                   callp(e)  UBISNUM_Check(PiStr
060300081113     C                                          :PiDecChr
060400081113     C                                          :PiVal
060500081113     C                                          :PiNum
060600081113     C                                          :PiInt)
060700081113     C*
060800000801     C                   IF        %error
060900000801     C                   EVAL      PiInt=*off
061000000801     C                   ENDIF
061100000801     C*
061200000801     C                   ENDSR
061300000801     C***
061400000801
061500011113
061600011113     C*----------------------------------------------------*
061700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061800011113     C*----------------------------------------------------*
061900011113     C     CHKIMPDIV     BEGSR
062000011113     C*
062100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062200011113     C                   Z-ADD     *zeros        wrkDec            9 9
062300011113     C*
062400011113     C* Come prima cosa effettuo considerazioni sulla divisa
062500011113     C                   IF        vabIAS > *zeros
062600011113     C                   IF        vabVAS <> 'EUR'
062700011113     C                   EVAL      vabVAS =  'ITL'
062800011113     C                   ENDIF
062900011113     C                   ENDIF
063000011113     C*
063100011113     C                   IF        vabCAS > *zeros
063200011113     C                   IF        vabVCA <> 'EUR'
063300011113     C                   EVAL      vabVCA =  'ITL'
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C                   IF        vabVMD > *zeros
063800020305     C                   IF        vabVAD <> 'EUR'
063900011113     C                   EVAL      vabVAD =  'ITL'
064000011113     C                   ENDIF
064100011113     C                   ENDIF
064200011113     C*
064300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064400011113     C                   Z-ADD     vabIAS        wrkDec
064500011113     C                   IF        wrkDec > *zeros
064600011113     C                   IF        vabVAS = 'ITL'
064700011113     C                   EVAL      vabIAS = *zeros
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065200011113     C                   Z-ADD     vabCAS        wrkDec
065300011113     C                   IF        wrkDec > *zeros
065400011113     C                   IF        vabVCA = 'ITL'
065500011113     C                   EVAL      vabCAS = *zeros
065600011113     C                   ENDIF
065700011113     C                   ENDIF
065800011113     C*
065900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066000011113     C                   Z-ADD     vabVMD        wrkDec
066100011113     C                   IF        wrkDec > *zeros
066200011113     C                   IF        vabVAD = 'ITL'
066300011113     C                   EVAL      vabVMD = *zeros
066400011113     C                   ENDIF
066500011113     C                   ENDIF
066600011113     C*
066700011113     C                   ENDSR
066800011113     C***
066900011113
067000011113
067100000801
067200000801
067300990920      /TITLE Invio dei dati al punto operativo.
067400010202     C     invio         BEGSR
067500990920     C*
067600021113     C* 1° invio FIVAT
067700010201     C                   reset                   dscmz
067800010201     C                   move      vlrpoi        cmzdst
067900021113     C                   eval      cmzfld = 'FIVATWWR'
068000010201     C                   eval      cmzmbd = vlrhdl
068100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068200021009     C***                if        prmfir = *blanks
068300021113     C                   eval      cmzfla = 'FIVAT00F'
068400021113     C                   eval      cmzmba = 'FIVAT00F'
068500021009     C***                else
068600021009     C***                eval      cmzfla = prmfir
068700021009     C***                eval      cmzmba = prmfir
068800021009     C***                endif
068900010201     C                   eval      cmznrr = *zeros
069000020305     C                   move      §ctrokvt      cmznrr
069100021018     C                   eval      cmzlba = vlrfl1
069200010201     C                   call(e)   'TIS711C'
069300010201     C                   parm                    dscmz
069400010201     C                   parm      *blanks       esito
069500010205     C                   if        %error
069600010205     C                             or cmzerr = '1'
069700010205     C                             or esito  = '1'
069800010205     C                   eval      wrkesito = '3'
069900010205     C                   else
070000010201     C*
070100021113     C* 2° invio FIVAB
070200010201     C                   reset                   dscmz
070300010201     C                   move      vlrpoi        cmzdst
070400010201     C                   eval      cmzfld = vlrfou
070500010201     C                   eval      cmzmbd = vlrhdl
070600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070700021009     C***                if        prmfir = *blanks
070800021113     C                   eval      cmzfla = 'FIVAB00F'
070900021113     C                   eval      cmzmba = 'FIVAB00F'
071000021009     C***                else
071100021009     C***                eval      cmzfla = prmfir
071200021009     C***                eval      cmzmba = prmfir
071300021009     C***                endif
071400010201     C                   eval      cmznrr = *zeros
071500010201     C                   move      §ctrokvb      cmznrr
071600021018     C                   eval      cmzlba = vlrfl1
071700010201     C                   call(e)   'TIS711C'
071800010201     C                   parm                    dscmz
071900010201     C                   parm      *blanks       esito
072000010201     C                   if        %error
072100010201     C                             or cmzerr = '1'
072200010201     C                             or esito  = '1'
072300010201     C                   eval      wrkesito = '3'
072400010201     C                   endif
072500010205     C                   endif
072600990920     C*
072700000613     C                   ENDSR
072800000613     C***
072900070411
073000070411     C     *pssr         BEGSR
073100070411     C*
073200070411     C                   if        %open(tivin00r)
073300070411     C                   close     tivin00r
073400070411     C                   endif
073500070411     C                   if        %open(fivabwwr)
073600070411     C                   close     fivabwwr
073700070411     C                   endif
073800070411     C                   if        %open(fivatwwr)
073900070411     C                   close     fivatwwr
074000070411     C                   endif
074100070411     C*
074200070411     C* Effettuo la chiamata al CLLE preposto
074300070411     C                   call(e)   'TITVVTC'
074400070411     C                   parm                    parccm
074500070411     C                   parm                    parmbr
074600070411     C                   parm      '2'           paropz
074700070411     C*
074800070411     C                   eval      wrkesito = '2'
074900070411     C*
075000070411     C                   seton                                        LR
075100070411     C*
075200070411     C                   ENDSR     '*CANCL'
075300070411     C***
075400070411
075500990910
075600000613     C     *inzsr        BEGSR
075700990910     C*
075800990910     C     *entry        plist
075900990920     C                   parm                    tivlrds
076000990921     C                   parm      wrkesito      esito
076100000724     C                   parm                    prmlit
076200000710     C                   parm                    prmfir
076300000613     C*
076400000830     C* CALCOLA LA DATA CORRENTE
076500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
076600091223     C                   eval      datcor = %dec(%date() : *ISO)
076700000830     C*
076800000613     C                   ENDSR
076900000613     C***
077000990908
077100100222     Otitv1v1p  E            riepilogo         2
077200990915     O                                              'Upload via Internet'
077300990915     O                                           +1 'Traduzione TIVIN00R -'
077400021113     O                                           +1 'FIVABWWR/FIVATWWR'
077500010201     O                                           +1 'Report testate bolle'
077600990915     O          E            riepilogo   2
077700990915     O                       wrkdata
077800990915     O                       wrkora              +1
077900990915     O                       procname            +1
078000990915     O          E            riepilogo   2
078100990915     O                                              'Cliente..................:'
078200990915     O                       VABCCM        z     +1
078300990915     O          E            riepilogo   2
078400990920     O                                              'Riferimento Strategi.....:'
078500990920     O                       vlrhdl              +1
078600990915     O          E            riepilogo   2
078700990915     O                                              'Giusti...................:'
078800010201     O                       §CTROKVB      2   +  1
078900990915     O          E            riepilogo   2
079000990915     O                                              'Sbagliati e corretti.....:'
079100971022     O                       §CTRMO        2   +  1
079200990915     O          E            riepilogo   2
079300990915     O                                              'Sbagliati e scartati.....:'
079400971022     O                       §CTRNO        2   +  1
079500000613
079600100222     Otitv1v1ps E            testdett          2
079700000613     O                                              'Upload via Internet'
079800000613     O                                           +1 'Traduzione TIVIN00R -'
079900021113     O                                           +1 'FIVABWWR/FIVATWWR'
080000010201     O                                           +1 'Report testate bolle'
080100000616     O          E            testdett    3
080200000613     O                                           +2 'N° rec'
080300000613     O                                           +3 'Anteprima contenuto'
080400000616     O          E            rigadett    2
080500000613     O                       rrnum               +2
080600000621     O                       recko               +3
080700000616     O          E            findett     2
080800000613     O                       wrkdata
080900000613     O                       wrkora              +1
081000000613     O                       procname            +1
081100000616     O          E            findett     2
081200000613     O                                              'Cliente..................:'
081300000613     O                       VABCCM        z     +1
081400000616     O          E            findett     2
081500000613     O                                              'Riferimento Strategi.....:'
081600000613     O                       vlrhdl              +1
081700000616     O          E            findett     2
081800000613     O                                              'Giusti...................:'
081900010201     O                       §CTROKVB      2   +  1
082000000616     O          E            findett     2
082100000613     O                                              'Sbagliati e corretti.....:'
082200000613     O                       §CTRMO        2   +  1
082300000616     O          E            findett     2
082400000613     O                                              'Sbagliati e scartati.....:'
082500000613     O                       §CTRNO        2   +  1
082600000512** CMD - COMANDI CL
082700100222OVRPRTF FILE(TITV1V1P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
082800100222OVRPRTF FILE(TITV1V1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
082900100222DLTOVR FILE(TITV1V1P TITV1V1PS) LVL(*)
083000000512
083100000512
