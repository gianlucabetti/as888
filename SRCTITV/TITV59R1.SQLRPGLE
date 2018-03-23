000100040301      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700040301     FFIVATwwr  O    E             DISK    usropn
000800030625     Ftitv59p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030625     Ftitv59ps  O    f  198        PRINTER usropn
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
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500030625     D depDest         s             97    INZ(*blanks)
003600030625     D curDest         s             97    INZ(*blanks)
003700010202     D parccm          s              8    INZ(*blanks)
003800010202     D parmbr          s             10    INZ(*blanks)
003900010202     D paropz          s              1    INZ(*blanks)
004000010202     D chkcall         s              1    INZ(*blanks)
004100020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004200040303     D depspe          s              6    INZ(*blanks)
004300010213
004400000801     D  Num5_0         s              5  0
004500000830
004600000830     D*------------------
004700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000830     D*------------------
004900000830     D WLBDA8          DS                  INZ
005000000830     D  G08DAT                 1      8  0
005100000830     D  G08INV                 9     16  0
005200000830     D  G08ERR                17     17
005300000830     D  G08TGI                18     22  0
005400000830     D*
005500020725
005600020725     D*------------------
005700030625     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005800020725     D*------------------
005900030625     D ADIDASDS        DS                  INZ
006000030625     D  VINDTA                 1   2048
006100030625     D  VINFLG              2049   2049
006200030625     D  VINDST              2050   2146
006300030625     D  VINRRN              2147   2154  0
006400020725     D*
006500990908
006600010201
006700990915     C                   time                    wrkdata
006800990915     C                   time                    wrkora
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200000724     C*
007300000724     C* SE OCCORRE SPEDIRE IN FILIALE
007400000724     C                   if        vlrpoi <> *zeros
007500000724     C*
007600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800000724     C     vlrpoi        chain     azorg01l
007900000724     C                   if        %found
008000000616     C                   movel(p)  CMD(1)        depcmd
008100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008200000616     C*
008300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008400011113     C                   Z-ADD     150           LENGH            15 5
008500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008600000616     C                   PARM                    depcmd
008700000616     C                   PARM                    LENGH
008800000724     C*
008900000724     C                   endif
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000616     C*
010200030625     C                   if        not %open(titv59ps)
010300030625     C                   open      titv59ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600000613     C*
010700040301     C                   EXSR      RWADI                                        LETT/SCR. VAB/VAT
010800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010900000613     C*
011000010202     C* Effettuo la chiamata al CLLE preposto
011100040301     C                   call(e)   'TITVVTC'
011200010202     C                   parm                    parccm
011300010202     C                   parm                    parmbr
011400010202     C                   parm      '2'           paropz
011500010202     C*
011600030625     C                   if        %open(titv59ps)
011700000616     C                   except    findett
011800030625     C                   close     titv59ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700000801     C
012800010201     C                   seton                                        LR
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500030625     C                   if        not %open(titv59p)
013600030625     C                   open      titv59p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100030625     C                   if        %open(titv59p)
014200030625     C                   close     titv59p
014300990914     C                   endif
014400990910     C*
014500000613     C                   ENDSR
014600000613     C***
014700990908
014800000801
014900910830     C*--------------------------------------------------------
015000040301     C* RWADI    LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR  *
015100910830     C*--------------------------------------------------------
015200030625     C     RWADI         BEGSR
015300990910     C*
015400990914     C                   if        not %open(tivin00r)
015500990908     C                   open      tivin00r
015600990914     C                   endif
015700021113     C                   if        not %open(fivabwwr)
015800021113     C                   open      fivabwwr
015900990914     C                   endif
016000040301     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016100040301     C                   exsr      prevat
016200010201     C*
016300010202     C                   if        chkcall = '0'
016400010202     C*
016500040301     C                   if        not %open(fivatwwr)
016600040301     C                   open      fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   clear                   §CTROKVB          5 0
017000020305     C                   clear                   §CTROKVT          5 0
017100000801     C                   clear                   §CTRMO            5 0
017200000801     C                   clear                   §CTRNO            5 0
017300990910     C*
017400020725     C*
017500020725     C/EXEC SQL
017600020725     C+ declare C1 cursor for select
017700030625     C+ vindta, vinflg, substr(vindta, 14, 97) as dest, rrn(tivin00r)
017800030625     C+ from tivin00r order by dest
017900020725     C+ for read only
018000020725     C/END-EXEC
018100020725     C
018200020725     C/EXEC SQL
018300020725     C+ open C1
018400020725     C/END-EXEC
018500020725     C
018600020725     C/EXEC SQL
018700030625     C+ Fetch C1 into :ADIDASDS
018800020725     C/END-EXEC
018900020725     C*
019000020725     C                   dow       sqlcod = *zeros
019100990913     C*
019200020725     C                   if        vindta > *blanks
019300000613     C                   add       1             rrnum
019400000801     C*
019500020725     C                   if        vinflg = *blanks
019600020725     C                             or vinflg = '0'
019700020725     C                             or vinflg = '2'
019800000801     C*
019900020725     C                   clear                   x_vinmsg
020000020725     C                   eval      x_vinflg = '1'
020100010305     C*
020200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
020300030625     C                   EVAL      curDest=%trim(%subst(vindta:14:97))
020400010305     C*
020500030625     C                   if        depDest = *blanks                            => 1° giro
020600030625     C                   eval      depDest = curDest                            => memorizz. dest.
020700020725     C                   clear                   tivinds
020800030625     C                   exsr      inzvar
020900030625     C                   exsr      defcam
021000020305     C                   exsr      impvab
021100040301     C                   exsr      wrivat                                       => carico VAT
021200010305     C                   else
021300030625     C                   if        depDest <> curDest                           => rottura di dest.
021400030625     C                   eval      depDest  = curDest                           => memorizz. dest.
021500020305     C                   exsr      wrivab
021600020725     C                   clear                   tivinds
021700020725     C                   exsr      inzvar
021800020725     C                   exsr      defcam
021900020305     C                   exsr      impvab
022000040301     C                   exsr      wrivat                                       => carico VAT
022100030625     C                   else                                                   => x stesso dest.
022200020305     C                   exsr      impvab
022300040301     C                   exsr      wrivat                                       => carico VAT
022400010305     C                   endif
022500010305     C                   endif
022600010305     C                   endif
022700000905     C*
022800000905     C                   else
022900020725     C                   eval      x_vinflg = '1'
023000000905     C                   endif
023100000905     C*
023200020725     C     VINRRN        chain     tivin000
023300020725     C                   if        %found(tivin00r)
023400020725     C                   eval      y_vinflg = x_vinflg
023500020725     C                   eval      y_vinmsg = x_vinmsg
023600020725     C                   update    tivin000
023700020725     C                   endif
023800020725     C*
023900020725     C/EXEC SQL
024000030625     C+ Fetch C1 into :ADIDASDS
024100020725     C/END-EXEC
024200020725     C*
024300020725     C                   enddo
024400020725     C*
024500020725     C/EXEC SQL
024600020725     C+ close C1
024700020725     C/END-EXEC
024800000905     C*
024900020305     C* Scarico i VAB rimasti "in sospeso"
025000020305     C                   exsr      wrivab
025100010202     C*
025200010202     C                   endif
025300990910
025400990910     C* Se non ci sono record con errori ...
025500000710     C                   if        §ctrno = 0
025600990910     C* ... restituisco esito OK.
025700990921     C                   eval      wrkesito = '0'
025800990910     C                   else
025900010201     C                   if        §ctrokvb > 0
026000990921     C                   eval      wrkesito = '1'
026100000710     C                   else
026200000710     C                   eval      wrkesito = '2'
026300990910     C                   endif
026400000710     C                   endif
026500990910     C*
026600990914     C                   if        %open(tivin00r)
026700990908     C                   close     tivin00r
026800990914     C                   endif
026900021113     C                   if        %open(fivabwwr)
027000021113     C                   close     fivabwwr
027100990914     C                   endif
027200040301     C                   if        %open(fivatwwr)
027300040301     C                   close     fivatwwr
027400010201     C                   endif
027500990910     C*
027600010201     C                   if        §ctrokvb > 0
027700000724     C                             and vlrpoi <> *zeros
027800010202     C                   exsr      invio
027900990920     C                   endif
028000990920     C*
028100910830     C                   ENDSR
028200000613     C***
028300010305
028400010305     C*----------------------------------------------------*
028500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028600010305     C*----------------------------------------------------*
028700020305     C     WRIVAB        BEGSR
028800010305     C*
028900021113     C                   write     fivab000                                     => scarico il VAB
029000010305     C*
029100010305     C                   ENDSR
029200990920
029300000801     C*----------------------------------------------------*
029400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500000801     C*----------------------------------------------------*
029600010201     C     INZVAR        BEGSR
029700000801     C*
029800010201     C                   Z-ADD     *zeros        Num5_0
029900000801     C*
030000000801     C                   ENDSR
030100000801     C*----------------------------------------------------*
030200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030300000801     C*----------------------------------------------------*
030400020904     C     DEFCAM        BEGSR
030500000801     C*
030600021113     C                   CLEAR                   FIVAB000
030700040301     C                   CLEAR                   FIVAT000
030800031202     C                   Z-ADD     0892639       VABCCM
030900040301     C                   Z-ADD     0892639       VATCCM
031000030625     C                   Z-ADD     089           VABLNP
031100040301     C                   Z-ADD     089           VATLNP
031200010201     C                   Z-ADD     000           VABCTR
031300040305     C                   MOVEL     '7Q'          VABCTM
031400030625     C                   MOVEL     'D'           VABTSP
031500040301     C                   MOVEL     'E'           VATTRC
031600040325     C* ... e poi verifico se sono stati passati come parametri
031700040325     C                   IF        vlrppt > *blanks
031800040325     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031900040325     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000040325     C                   EXSR      CHKNUM
032100040325     C                   IF        PiInt=*on
032200040325     C                   Z-ADD     PiVal         VABCCM
032300040329     C                   Z-ADD     PiVal         VATCCM
032400040325     C                   ENDIF
032500040325     C                   ENDIF
032600040325     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032700040325     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800040325     C                   EXSR      CHKNUM
032900040325     C                   IF        PiInt=*on
033000040325     C                   Z-ADD     PiVal         VABLNP
033100040329     C                   Z-ADD     PiVal         VATLNP
033200040325     C                   ENDIF
033300040325     C                   ENDIF
033400040325     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033500040325     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033600040325     C                   EXSR      CHKNUM
033700040325     C                   IF        PiInt=*on
033800040325     C                   Z-ADD     PiVal         VABCTR
033900040325     C                   ENDIF
034000040325     C                   ENDIF
034100040325     C                   ENDIF
034200000801     C*
034300000801     C                   ENDSR
034400000801     C*----------------------------------------------------*
034500030625     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034600000801     C*----------------------------------------------------*
034700010201     C     IMPVAB        BEGSR
034800010305     C*
034900000801     C                   Z-ADD     *zeros        errore            1 0
035000021113     C*
035100021113     C                   MOVE(P)   vlrpoi        VABFGS
035200040301     C                   MOVE(P)   vlrpoi        VATFGS
035300020725     C*
035400020725     C* Campi anno e mese/giorno
035500030625     C                   MOVEL     datcor        VABAAS
035600040301     C                   MOVEL     datcor        VATAAS
035700030625     C                   MOVE(P)   datcor        VABMGS
035800020725     C*
035900020725     C* Campi relativi al destinatario
036000030625     C                   EVAL      VABRSD=%trim(%subst(vindta:14:35))
036100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
036300020117     C* ==
036400030625     C                   EVAL      VABIND=%trim(%subst(vindta:49:35))
036500030625     C                   EVAL      VABLOD=%trim(%subst(vindta:84:20))
036600030625     C                   EVAL      VABPRD=%trim(%subst(vindta:109:2))
036700030625     C                   EVAL      VABRMA=%trim(%subst(vindta:133:6))
036800040301     C                   EVAL      VATNOT=%trim(%subst(vindta:154:15))
036900030625     C                   EVAL      VABCBO = '1'
037000020725     C*
037100030625     C* CAD
037200030625     C                   EVAL      PiStr=%trim(%subst(vindta:104:5))
037300000801     C                   EXSR      CHKNUM
037400000801     C                   IF        PiInt=*on
037500000801     C                   Z-ADD     PiVal         Num5_0
037600010208     C                   MOVEL(p)  Num5_0        VABCAD
037700000801     C                   ELSE
037800000801     C                   ADD       1             errore
037900020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038000000801     C                             + ' ' + 'VABCAD'
038100000801     C                   ENDIF
038200030625     C* NCL
038300040301     C                   ADD       1             VABNCL
038400040303     C* PKB (lo sommo solo al cambio d spedizine x medesimo destinatario)
038500030625     C                   EVAL      PiStr=%trim(%subst(vindta:142:6))
038600010201     C                   EXSR      CHKNUM
038700010201     C                   IF        PiNum=*on
038800040715     C                   IF        %subst(vindta:114:6) <> depspe
038900040715     C                   EVAL      depspe = %subst(vindta:114:6)
039000040715     C                   EVAL      VABPKB = PiVal
039100040715     C                   ELSE
039200040715     C                   EVAL      VABPKB = VABPKB + PiVal
039300040715     C                   ENDIF
039400010201     C                   ELSE
039500010201     C                   ADD       1             errore
039600020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039700010201     C                             + ' ' + 'VABPKB'
039800040303     C                   ENDIF
039900030625     C* NSP/RMN (lo valorizzo solo se 0 x gestire la rottura di codice x destinatario)
040000030625     C                   IF        VABNSP = *zeros
040100030625     C                   EVAL      PiStr=%trim(%subst(vindta:114:6))
040200010201     C                   EXSR      CHKNUM
040300010201     C                   IF        PiInt=*on
040400011115     C                   Z-ADD     PiVal         VABNSP
040500030625     C                   Z-ADD     PiVal         VABRMN
040600040301     C                   Z-ADD     PiVal         VATNSP
040700010201     C                   ELSE
040800010201     C                   ADD       1             errore
040900020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041000040301     C                             + ' ' + 'VABNSP/VABRMN/VATNSP'
041100010201     C                   ENDIF
041200020904     C                   ENDIF
041300020305     C*
041400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041500011113     C                   EXSR      CHKIMPDIV
041600010202     C*
041700000801     C* Ebbene...
041800000801     C                   ADD       1             §CTRMO
041900010201     C                   IF        errore <> *zeros
042000000801     C                   ADD       1             §CTRNO
042100000801     C                   EVAL      recko = vindta
042200000801     C                   EXCEPT    rigadett
042300020725     C                   EVAL      x_vinflg = '2'
042400000801     C                   ELSE
042500010201     C                   ADD       1             §CTROKVB
042600000801     C                   ENDIF
042700000801     C*
042800000801     C                   ENDSR
042900010201     C*----------------------------------------------------*
043000040301     C*  SCARICAMENTO BUFFER RECORDS VAT
043100010201     C*----------------------------------------------------*
043200040301     C     WRIVAT        BEGSR
043300010201     C*
043400040301     C                   write     FIVAT000
043500010201     C*
043600010201     C                   ENDSR
043700020904
043800020904
043900010202     C*----------------------------------------------------*
044000040301     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044100010202     C*----------------------------------------------------*
044200040301     C     PREVAT        BEGSR
044300010202     C*
044400040301     C* Compongo il nome del membro da dare al FIVATWWR
044500010202     C                   eval      parmbr = vlrhdl
044600010202     C                   movel     'M'           parmbr
044700030625     C                   eval      parccm = '00891281'
044800010202     C                   eval      paropz = '1'
044900010202     C* Effettuo la chiamata al CLLE preposto
045000040301     C                   call(e)   'TITVVTC'
045100010202     C                   parm                    parccm
045200010202     C                   parm                    parmbr
045300010202     C                   parm                    paropz
045400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045500010202     C                   if        %error
045600010202     C                   movel     '1'           chkcall
045700010202     C                   else
045800010202     C                   movel     '0'           chkcall
045900010202     C                   endif
046000010202     C*
046100010202     C                   ENDSR
046200000801     C*----------------------------------------------------*
046300000801     C*  CONTROLLO NUMERICITA' CAMPI
046400000801     C*----------------------------------------------------*
046500000801     C     CHKNUM        BEGSR
046600000801     C*
046700000801     C                   call(e)   'ISNUMERIC'
046800000801     C                   PARM                    PiStr            30
046900000801     C                   PARM      ','           PiDecChr          1
047000000801     C                   PARM      *ZEROS        PiVal            30 9
047100000801     C                   PARM      '0'           PiInt             1
047200000801     C                   PARM      '0'           PiNum             1
047300000801     C                   IF        %error
047400000801     C                   EVAL      PiInt=*off
047500000801     C                   ENDIF
047600000801     C*
047700000801     C                   ENDSR
047800000801     C***
047900000801
048000011113
048100011113     C*----------------------------------------------------*
048200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048300011113     C*----------------------------------------------------*
048400011113     C     CHKIMPDIV     BEGSR
048500011113     C*
048600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048700011113     C                   Z-ADD     *zeros        wrkDec            9 9
048800011113     C*
048900011113     C* Come prima cosa effettuo considerazioni sulla divisa
049000011113     C                   IF        vabIAS > *zeros
049100011113     C                   IF        vabVAS <> 'EUR'
049200011113     C                   EVAL      vabVAS =  'ITL'
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C                   IF        vabCAS > *zeros
049700011113     C                   IF        vabVCA <> 'EUR'
049800011113     C                   EVAL      vabVCA =  'ITL'
049900011113     C                   ENDIF
050000011113     C                   ENDIF
050100011113     C*
050200011113     C                   IF        vabVMD > *zeros
050300020305     C                   IF        vabVAD <> 'EUR'
050400011113     C                   EVAL      vabVAD =  'ITL'
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050900011113     C                   Z-ADD     vabIAS        wrkDec
051000011113     C                   IF        wrkDec > *zeros
051100011113     C                   IF        vabVAS = 'ITL'
051200011113     C                   EVAL      vabIAS = *zeros
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051700011113     C                   Z-ADD     vabCAS        wrkDec
051800011113     C                   IF        wrkDec > *zeros
051900011113     C                   IF        vabVCA = 'ITL'
052000011113     C                   EVAL      vabCAS = *zeros
052100011113     C                   ENDIF
052200011113     C                   ENDIF
052300011113     C*
052400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052500011113     C                   Z-ADD     vabVMD        wrkDec
052600011113     C                   IF        wrkDec > *zeros
052700011113     C                   IF        vabVAD = 'ITL'
052800011113     C                   EVAL      vabVMD = *zeros
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C                   ENDSR
053300011113     C***
053400011113
053500011113
053600000801
053700000801
053800990920      /TITLE Invio dei dati al punto operativo.
053900010202     C     invio         BEGSR
054000990920     C*
054100040301     C* 1° invio FIVAT
054200010201     C                   reset                   dscmz
054300010201     C                   move      vlrpoi        cmzdst
054400040301     C                   eval      cmzfld = 'FIVATWWR'
054500010201     C                   eval      cmzmbd = vlrhdl
054600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054700021009     C***                if        prmfir = *blanks
054800040301     C                   eval      cmzfla = 'FIVAT00F'
054900040301     C                   eval      cmzmba = 'FIVAT00F'
055000021009     C***                else
055100021009     C***                eval      cmzfla = prmfir
055200021009     C***                eval      cmzmba = prmfir
055300021009     C***                endif
055400010201     C                   eval      cmznrr = *zeros
055500020305     C                   move      §ctrokvt      cmznrr
055600021018     C                   eval      cmzlba = vlrfl1
055700010201     C                   call(e)   'TIS711C'
055800010201     C                   parm                    dscmz
055900010201     C                   parm      *blanks       esito
056000010205     C                   if        %error
056100010205     C                             or cmzerr = '1'
056200010205     C                             or esito  = '1'
056300010205     C                   eval      wrkesito = '3'
056400010205     C                   else
056500010201     C*
056600021113     C* 2° invio FIVAB
056700010201     C                   reset                   dscmz
056800010201     C                   move      vlrpoi        cmzdst
056900010201     C                   eval      cmzfld = vlrfou
057000010201     C                   eval      cmzmbd = vlrhdl
057100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057200021009     C***                if        prmfir = *blanks
057300021113     C                   eval      cmzfla = 'FIVAB00F'
057400021113     C                   eval      cmzmba = 'FIVAB00F'
057500021009     C***                else
057600021009     C***                eval      cmzfla = prmfir
057700021009     C***                eval      cmzmba = prmfir
057800021009     C***                endif
057900010201     C                   eval      cmznrr = *zeros
058000010201     C                   move      §ctrokvb      cmznrr
058100021018     C                   eval      cmzlba = vlrfl1
058200010201     C                   call(e)   'TIS711C'
058300010201     C                   parm                    dscmz
058400010201     C                   parm      *blanks       esito
058500010201     C                   if        %error
058600010201     C                             or cmzerr = '1'
058700010201     C                             or esito  = '1'
058800010201     C                   eval      wrkesito = '3'
058900010201     C                   endif
059000010205     C                   endif
059100990920     C*
059200000613     C                   ENDSR
059300000613     C***
059400070411
059500070411     C     *pssr         BEGSR
059600070411     C*
059700070411     C                   if        %open(tivin00r)
059800070411     C                   close     tivin00r
059900070411     C                   endif
060000070411     C                   if        %open(fivabwwr)
060100070411     C                   close     fivabwwr
060200070411     C                   endif
060300070411     C                   if        %open(fivatwwr)
060400070411     C                   close     fivatwwr
060500070411     C                   endif
060600070411     C*
060700070411     C* Effettuo la chiamata al CLLE preposto
060800070411     C                   call(e)   'TITVVTC'
060900070411     C                   parm                    parccm
061000070411     C                   parm                    parmbr
061100070411     C                   parm      '2'           paropz
061200070411     C*
061300070411     C                   eval      wrkesito = '2'
061400070411     C*
061500070411     C                   seton                                        LR
061600070411     C*
061700070411     C                   ENDSR     '*CANCL'
061800070411     C***
061900070411
062000990910
062100000613     C     *inzsr        BEGSR
062200990910     C*
062300990910     C     *entry        plist
062400990920     C                   parm                    tivlrds
062500990921     C                   parm      wrkesito      esito
062600000724     C                   parm                    prmlit
062700000710     C                   parm                    prmfir
062800000613     C*
062900000830     C* CALCOLA LA DATA CORRENTE
063000000830     C                   time                    wn14             14 0
063100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063200000830     C                   z-add     wn8           g08dat
063300000830     C                   z-add     *zeros        g08inv
063400000830     C                   movel     '0'           g08err
063500000830     C                   call      'XSRDA8'
063600000830     C                   parm                    wlbda8
063700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063800000830     C*
063900000613     C                   ENDSR
064000000613     C***
064100990908
064200030625     Otitv59p   E            riepilogo         2
064300990915     O                                              'Upload via Internet'
064400990915     O                                           +1 'Traduzione TIVIN00R -'
064500040301     O                                           +1 'FIVABWWR/FIVATWWR'
064600010201     O                                           +1 'Report testate bolle'
064700990915     O          E            riepilogo   2
064800990915     O                       wrkdata
064900990915     O                       wrkora              +1
065000990915     O                       procname            +1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Cliente..................:'
065300990915     O                       VABCCM        z     +1
065400990915     O          E            riepilogo   2
065500990920     O                                              'Riferimento Strategi.....:'
065600990920     O                       vlrhdl              +1
065700990915     O          E            riepilogo   2
065800990915     O                                              'Giusti...................:'
065900010201     O                       §CTROKVB      2   +  1
066000990915     O          E            riepilogo   2
066100990915     O                                              'Sbagliati e corretti.....:'
066200971022     O                       §CTRMO        2   +  1
066300990915     O          E            riepilogo   2
066400990915     O                                              'Sbagliati e scartati.....:'
066500971022     O                       §CTRNO        2   +  1
066600000613
066700030625     Otitv59ps  E            testdett          2
066800000613     O                                              'Upload via Internet'
066900000613     O                                           +1 'Traduzione TIVIN00R -'
067000040301     O                                           +1 'FIVABWWR/FIVATWWR'
067100010201     O                                           +1 'Report testate bolle'
067200000616     O          E            testdett    3
067300000613     O                                           +2 'N° rec'
067400000613     O                                           +3 'Anteprima contenuto'
067500000616     O          E            rigadett    2
067600000613     O                       rrnum               +2
067700000621     O                       recko               +3
067800000616     O          E            findett     2
067900000613     O                       wrkdata
068000000613     O                       wrkora              +1
068100000613     O                       procname            +1
068200000616     O          E            findett     2
068300000613     O                                              'Cliente..................:'
068400000613     O                       VABCCM        z     +1
068500000616     O          E            findett     2
068600000613     O                                              'Riferimento Strategi.....:'
068700000613     O                       vlrhdl              +1
068800000616     O          E            findett     2
068900000613     O                                              'Giusti...................:'
069000010201     O                       §CTROKVB      2   +  1
069100000616     O          E            findett     2
069200000613     O                                              'Sbagliati e corretti.....:'
069300000613     O                       §CTRMO        2   +  1
069400000616     O          E            findett     2
069500000613     O                                              'Sbagliati e scartati.....:'
069600000613     O                       §CTRNO        2   +  1
069700000512** CMD - COMANDI CL
069800030625OVRPRTF FILE(TITV59P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
069900030625OVRPRTF FILE(TITV59PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
070000030625DLTOVR FILE(TITV59P TITV59PS) LVL(*)
070100000512
070200000512
