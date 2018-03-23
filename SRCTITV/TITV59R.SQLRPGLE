000100030625      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVADWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700030625     FFIVADwwr  O    E             DISK    usropn
000800030625     Ftitv59p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000030625     Ftitv59ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000801     D*----------------------------------------------------
001800000801     D* DICHIARAZIOINE VARIABILI DI WRK
001900000801     D*----------------------------------------------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800990915     D wrkdata         s               d
002900990915     D wrkora          s               t
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100000621     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300030625     D depDest         s             97    INZ(*blanks)
003400030625     D curDest         s             97    INZ(*blanks)
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004000010213
004100000801     D  Num5_0         s              5  0
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100000830     D*
005200020725
005300020725     D*------------------
005400030625     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005500020725     D*------------------
005600030625     D ADIDASDS        DS                  INZ
005700030625     D  VINDTA                 1   2048
005800030625     D  VINFLG              2049   2049
005900030625     D  VINDST              2050   2146
006000030625     D  VINRRN              2147   2154  0
006100020725     D*
006200990908
006300010201
006400990915     C                   time                    wrkdata
006500990915     C                   time                    wrkora
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000724     C*
007000000724     C* SE OCCORRE SPEDIRE IN FILIALE
007100000724     C                   if        vlrpoi <> *zeros
007200000724     C*
007300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007500000724     C     vlrpoi        chain     azorg01l
007600000724     C                   if        %found
007700000616     C                   movel(p)  CMD(1)        depcmd
007800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000724     C*
008600000724     C                   endif
008700000724     C                   endif
008800000616     C*
008900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009000000616     C                   movel(p)  CMD(2)        depcmd
009100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009200000616     C*
009300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009400011113     C                   Z-ADD     150           LENGH            15 5
009500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000616     C                   PARM                    depcmd
009700000616     C                   PARM                    LENGH
009800000616     C*
009900030625     C                   if        not %open(titv59ps)
010000030625     C                   open      titv59ps
010100000616     C                   except    testdett
010200000613     C                   endif
010300000613     C*
010400030625     C                   EXSR      RWADI                                        LETT/SCR. VAB/VAD
010500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010600000616     C*
010700081006     C                   exsr      endela
010800081006     C*
010900000801     C
011000010201     C                   seton                                        LR
011100000613
011200000613     C*--------------------------------------------------------
011300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
011400000613     C*--------------------------------------------------------
011500000621     C     STPR          BEGSR
011600000613     C*
011700030625     C                   if        not %open(titv59p)
011800030625     C                   open      titv59p
011900990915     C                   endif
012000990915     C*
012100990915     C                   except    riepilogo
012200990915     C*
012300030625     C                   if        %open(titv59p)
012400030625     C                   close     titv59p
012500990914     C                   endif
012600990910     C*
012700000613     C                   ENDSR
012800000613     C***
012900990908
013000000801
013100910830     C*--------------------------------------------------------
013200030625     C* RWADI    LEGGE tivin00r E SCRIVE FIVABWWR e FIVADWWR  *
013300910830     C*--------------------------------------------------------
013400030625     C     RWADI         BEGSR
013500990910     C*
013600990914     C                   if        not %open(tivin00r)
013700990908     C                   open      tivin00r
013800990914     C                   endif
013900021113     C                   if        not %open(fivabwwr)
014000021113     C                   open      fivabwwr
014100990914     C                   endif
014200030625     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR
014300030625     C                   exsr      prevad
014400010201     C*
014500010202     C                   if        chkcall = '0'
014600010202     C*
014700030625     C                   if        not %open(fivadwwr)
014800030625     C                   open      fivadwwr
014900010201     C                   endif
015000990910     C*
015100010201     C                   clear                   §CTROKVB          5 0
015200020305     C                   clear                   §CTROKVT          5 0
015300000801     C                   clear                   §CTRMO            5 0
015400000801     C                   clear                   §CTRNO            5 0
015500990910     C*
015600020725     C*
015700020725     C/EXEC SQL
015800020725     C+ declare C1 cursor for select
015900030625     C+ vindta, vinflg, substr(vindta, 14, 97) as dest, rrn(tivin00r)
016000030625     C+ from tivin00r order by dest
016100020725     C+ for read only
016200020725     C/END-EXEC
016300020725     C
016400020725     C/EXEC SQL
016500020725     C+ open C1
016600020725     C/END-EXEC
016700020725     C
016800020725     C/EXEC SQL
016900030625     C+ Fetch C1 into :ADIDASDS
017000020725     C/END-EXEC
017100020725     C*
017200020725     C                   dow       sqlcod = *zeros
017300990913     C*
017400020725     C                   if        vindta > *blanks
017500000613     C                   add       1             rrnum
017600000801     C*
017700020725     C                   if        vinflg = *blanks
017800020725     C                             or vinflg = '0'
017900020725     C                             or vinflg = '2'
018000000801     C*
018100020725     C                   clear                   x_vinmsg
018200020725     C                   eval      x_vinflg = '1'
018300010305     C*
018400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018500030625     C                   EVAL      curDest=%trim(%subst(vindta:14:97))
018600010305     C*
018700030625     C                   if        depDest = *blanks                            => 1° giro
018800030625     C                   eval      depDest = curDest                            => memorizz. dest.
018900020725     C                   clear                   tivinds
019000030625     C                   exsr      inzvar
019100030625     C                   exsr      defcam
019200020305     C                   exsr      impvab
019300030625     C                   exsr      wrivad                                       => carico VAD
019400010305     C                   else
019500030625     C                   if        depDest <> curDest                           => rottura di dest.
019600030625     C                   eval      depDest  = curDest                           => memorizz. dest.
019700020305     C                   exsr      wrivab
019800020725     C                   clear                   tivinds
019900020725     C                   exsr      inzvar
020000020725     C                   exsr      defcam
020100020305     C                   exsr      impvab
020200030625     C                   exsr      wrivad                                       => carico VAD
020300030625     C                   else                                                   => x stesso dest.
020400020305     C                   exsr      impvab
020500030625     C                   exsr      wrivad                                       => carico VAD
020600010305     C                   endif
020700010305     C                   endif
020800010305     C                   endif
020900000905     C*
021000000905     C                   else
021100020725     C                   eval      x_vinflg = '1'
021200000905     C                   endif
021300000905     C*
021400020725     C     VINRRN        chain     tivin000
021500020725     C                   if        %found(tivin00r)
021600020725     C                   eval      y_vinflg = x_vinflg
021700020725     C                   eval      y_vinmsg = x_vinmsg
021800020725     C                   update    tivin000
021900020725     C                   endif
022000020725     C*
022100020725     C/EXEC SQL
022200030625     C+ Fetch C1 into :ADIDASDS
022300020725     C/END-EXEC
022400020725     C*
022500020725     C                   enddo
022600020725     C*
022700020725     C/EXEC SQL
022800020725     C+ close C1
022900020725     C/END-EXEC
023000000905     C*
023100020305     C* Scarico i VAB rimasti "in sospeso"
023200020305     C                   exsr      wrivab
023300010202     C*
023400010202     C                   endif
023500990910
023600990910     C* Se non ci sono record con errori ...
023700000710     C                   if        §ctrno = 0
023800990910     C* ... restituisco esito OK.
023900990921     C                   eval      wrkesito = '0'
024000990910     C                   else
024100010201     C                   if        §ctrokvb > 0
024200990921     C                   eval      wrkesito = '1'
024300000710     C                   else
024400000710     C                   eval      wrkesito = '2'
024500990910     C                   endif
024600000710     C                   endif
024700990910     C*
024800010201     C                   if        §ctrokvb > 0
024900000724     C                             and vlrpoi <> *zeros
025000010202     C                   exsr      invio
025100990920     C                   endif
025200990920     C*
025300910830     C                   ENDSR
025400000613     C***
025500010305
025600010305     C*----------------------------------------------------*
025700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025800010305     C*----------------------------------------------------*
025900020305     C     WRIVAB        BEGSR
026000010305     C*
026100021113     C                   write     fivab000                                     => scarico il VAB
026200010305     C*
026300010305     C                   ENDSR
026400990920
026500000801     C*----------------------------------------------------*
026600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026700000801     C*----------------------------------------------------*
026800010201     C     INZVAR        BEGSR
026900000801     C*
027000010201     C                   Z-ADD     *zeros        Num5_0
027100000801     C*
027200000801     C                   ENDSR
027300000801     C*----------------------------------------------------*
027400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027500000801     C*----------------------------------------------------*
027600020904     C     DEFCAM        BEGSR
027700000801     C*
027800021113     C                   CLEAR                   FIVAB000
027900030625     C                   CLEAR                   FIVAD000
028000031202     C                   Z-ADD     0892639       VABCCM
028100031202     C                   Z-ADD     0892639       VADCCM
028200030625     C                   Z-ADD     089           VABLNP
028300030625     C                   Z-ADD     089           VADLNP
028400010201     C                   Z-ADD     000           VABCTR
028500030625     C                   MOVEL     'DT'          VABCTM
028600030625     C                   MOVEL     'D'           VABTSP
028700000801     C*
028800000801     C                   ENDSR
028900000801     C*----------------------------------------------------*
029000030625     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029100000801     C*----------------------------------------------------*
029200010201     C     IMPVAB        BEGSR
029300010305     C*
029400000801     C                   Z-ADD     *zeros        errore            1 0
029500021113     C*
029600021113     C                   MOVE(P)   vlrpoi        VABFGS
029700030625     C                   MOVE(P)   vlrpoi        VADFGS
029800020725     C*
029900020725     C* Campi anno e mese/giorno
030000030625     C                   MOVEL     datcor        VABAAS
030100030625     C                   MOVEL     datcor        VADAAS
030200030625     C                   MOVE(P)   datcor        VABMGS
030300020725     C*
030400020725     C* Campi relativi al destinatario
030500030625     C                   EVAL      VABRSD=%trim(%subst(vindta:14:35))
030600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030800020117     C* ==
030900030625     C                   EVAL      VABIND=%trim(%subst(vindta:49:35))
031000030625     C                   EVAL      VABLOD=%trim(%subst(vindta:84:20))
031100030625     C                   EVAL      VABPRD=%trim(%subst(vindta:109:2))
031200030625     C                   EVAL      VABRMA=%trim(%subst(vindta:133:6))
031300030625     C                   EVAL      VADCDP=%trim(%subst(vindta:133:6))
031400030625     C                   EVAL      VABCBO = '1'
031500020725     C*
031600030625     C* CAD
031700030625     C                   EVAL      PiStr=%trim(%subst(vindta:104:5))
031800000801     C                   EXSR      CHKNUM
031900000801     C                   IF        PiInt=*on
032000000801     C                   Z-ADD     PiVal         Num5_0
032100010208     C                   MOVEL(p)  Num5_0        VABCAD
032200000801     C                   ELSE
032300000801     C                   ADD       1             errore
032400020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
032500000801     C                             + ' ' + 'VABCAD'
032600000801     C                   ENDIF
032700030625     C* NCL
032800030625     C                   EVAL      PiStr=%trim(%subst(vindta:139:3))
032900010201     C                   EXSR      CHKNUM
033000010201     C                   IF        PiInt=*on
033100020725     C                   ADD       PiVal         VABNCL
033200030625     C                   Z-ADD     PiVal         VADNCL
033300010201     C                   ELSE
033400010201     C                   ADD       1             errore
033500020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
033600030625     C                             + ' ' + 'VABNCL/VADNCL'
033700010201     C                   ENDIF
033800030625     C* PKB
033900030625     C                   EVAL      PiStr=%trim(%subst(vindta:142:6))
034000010201     C                   EXSR      CHKNUM
034100010201     C                   IF        PiNum=*on
034200030625     C                   EVAL      VABPKB = VABPKB + PiVal
034300010201     C                   ELSE
034400010201     C                   ADD       1             errore
034500020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
034600010201     C                             + ' ' + 'VABPKB'
034700010201     C                   ENDIF
034800030625     C* NSP/RMN (lo valorizzo solo se 0 x gestire la rottura di codice x destinatario)
034900030625     C                   IF        VABNSP = *zeros
035000030625     C                   EVAL      PiStr=%trim(%subst(vindta:114:6))
035100010201     C                   EXSR      CHKNUM
035200010201     C                   IF        PiInt=*on
035300011115     C                   Z-ADD     PiVal         VABNSP
035400030625     C                   Z-ADD     PiVal         VABRMN
035500030625     C                   Z-ADD     PiVal         VADNSP
035600010201     C                   ELSE
035700010201     C                   ADD       1             errore
035800020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035900030625     C                             + ' ' + 'VABNSP/VABRMN/VADNSP'
036000010201     C                   ENDIF
036100020904     C                   ENDIF
036200020305     C*
036300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036400011113     C                   EXSR      CHKIMPDIV
036500010202     C*
036600000801     C* Ebbene...
036700000801     C                   ADD       1             §CTRMO
036800010201     C                   IF        errore <> *zeros
036900000801     C                   ADD       1             §CTRNO
037000000801     C                   EVAL      recko = vindta
037100000801     C                   EXCEPT    rigadett
037200020725     C                   EVAL      x_vinflg = '2'
037300000801     C                   ELSE
037400010201     C                   ADD       1             §CTROKVB
037500000801     C                   ENDIF
037600000801     C*
037700000801     C                   ENDSR
037800010201     C*----------------------------------------------------*
037900030625     C*  SCARICAMENTO BUFFER RECORDS VAD
038000010201     C*----------------------------------------------------*
038100030625     C     WRIVAD        BEGSR
038200010201     C*
038300030625     C                   write     FIVAD000
038400010201     C*
038500010201     C                   ENDSR
038600020904
038700020904
038800010202     C*----------------------------------------------------*
038900030625     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR
039000010202     C*----------------------------------------------------*
039100030625     C     PREVAD        BEGSR
039200010202     C*
039300030625     C* Compongo il nome del membro da dare al FIVADWWR
039400010202     C                   eval      parmbr = vlrhdl
039500010202     C                   movel     'M'           parmbr
039600030625     C                   eval      parccm = '00891281'
039700010202     C                   eval      paropz = '1'
039800010202     C* Effettuo la chiamata al CLLE preposto
039900030625     C                   call(e)   'TITVVDC'
040000010202     C                   parm                    parccm
040100010202     C                   parm                    parmbr
040200010202     C                   parm                    paropz
040300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040400010202     C                   if        %error
040500010202     C                   movel     '1'           chkcall
040600010202     C                   else
040700010202     C                   movel     '0'           chkcall
040800010202     C                   endif
040900010202     C*
041000010202     C                   ENDSR
041100000801     C*----------------------------------------------------*
041200000801     C*  CONTROLLO NUMERICITA' CAMPI
041300000801     C*----------------------------------------------------*
041400000801     C     CHKNUM        BEGSR
041500000801     C*
041600000801     C                   call(e)   'ISNUMERIC'
041700000801     C                   PARM                    PiStr            30
041800000801     C                   PARM      ','           PiDecChr          1
041900000801     C                   PARM      *ZEROS        PiVal            30 9
042000000801     C                   PARM      '0'           PiInt             1
042100000801     C                   PARM      '0'           PiNum             1
042200000801     C                   IF        %error
042300000801     C                   EVAL      PiInt=*off
042400000801     C                   ENDIF
042500000801     C*
042600000801     C                   ENDSR
042700000801     C***
042800000801
042900011113
043000011113     C*----------------------------------------------------*
043100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043200011113     C*----------------------------------------------------*
043300011113     C     CHKIMPDIV     BEGSR
043400011113     C*
043500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043600011113     C                   Z-ADD     *zeros        wrkDec            9 9
043700011113     C*
043800011113     C* Come prima cosa effettuo considerazioni sulla divisa
043900011113     C                   IF        vabIAS > *zeros
044000011113     C                   IF        vabVAS <> 'EUR'
044100011113     C                   EVAL      vabVAS =  'ITL'
044200011113     C                   ENDIF
044300011113     C                   ENDIF
044400011113     C*
044500011113     C                   IF        vabCAS > *zeros
044600011113     C                   IF        vabVCA <> 'EUR'
044700011113     C                   EVAL      vabVCA =  'ITL'
044800011113     C                   ENDIF
044900011113     C                   ENDIF
045000011113     C*
045100011113     C                   IF        vabVMD > *zeros
045200020305     C                   IF        vabVAD <> 'EUR'
045300011113     C                   EVAL      vabVAD =  'ITL'
045400011113     C                   ENDIF
045500011113     C                   ENDIF
045600011113     C*
045700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045800011113     C                   Z-ADD     vabIAS        wrkDec
045900011113     C                   IF        wrkDec > *zeros
046000011113     C                   IF        vabVAS = 'ITL'
046100011113     C                   EVAL      vabIAS = *zeros
046200011113     C                   ENDIF
046300011113     C                   ENDIF
046400011113     C*
046500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
046600011113     C                   Z-ADD     vabCAS        wrkDec
046700011113     C                   IF        wrkDec > *zeros
046800011113     C                   IF        vabVCA = 'ITL'
046900011113     C                   EVAL      vabCAS = *zeros
047000011113     C                   ENDIF
047100011113     C                   ENDIF
047200011113     C*
047300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047400011113     C                   Z-ADD     vabVMD        wrkDec
047500011113     C                   IF        wrkDec > *zeros
047600011113     C                   IF        vabVAD = 'ITL'
047700011113     C                   EVAL      vabVMD = *zeros
047800011113     C                   ENDIF
047900011113     C                   ENDIF
048000011113     C*
048100011113     C                   ENDSR
048200011113     C***
048300011113
048400011113
048500000801
048600000801
048700990920      /TITLE Invio dei dati al punto operativo.
048800010202     C     invio         BEGSR
048900990920     C*
049000030625     C* 1° invio FIVAD
049100010201     C                   reset                   dscmz
049200010201     C                   move      vlrpoi        cmzdst
049300030625     C                   eval      cmzfld = 'FIVADWWR'
049400010201     C                   eval      cmzmbd = vlrhdl
049500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049600021009     C***                if        prmfir = *blanks
049700030625     C                   eval      cmzfla = 'FIVAD00F'
049800030625     C                   eval      cmzmba = 'FIVAD00F'
049900021009     C***                else
050000021009     C***                eval      cmzfla = prmfir
050100021009     C***                eval      cmzmba = prmfir
050200021009     C***                endif
050300010201     C                   eval      cmznrr = *zeros
050400020305     C                   move      §ctrokvt      cmznrr
050500021018     C                   eval      cmzlba = vlrfl1
050600010201     C                   call(e)   'TIS711C'
050700010201     C                   parm                    dscmz
050800010201     C                   parm      *blanks       esito
050900010205     C                   if        %error
051000010205     C                             or cmzerr = '1'
051100010205     C                             or esito  = '1'
051200010205     C                   eval      wrkesito = '3'
051300010205     C                   else
051400010201     C*
051500021113     C* 2° invio FIVAB
051600010201     C                   reset                   dscmz
051700010201     C                   move      vlrpoi        cmzdst
051800010201     C                   eval      cmzfld = vlrfou
051900010201     C                   eval      cmzmbd = vlrhdl
052000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052100021009     C***                if        prmfir = *blanks
052200021113     C                   eval      cmzfla = 'FIVAB00F'
052300021113     C                   eval      cmzmba = 'FIVAB00F'
052400021009     C***                else
052500021009     C***                eval      cmzfla = prmfir
052600021009     C***                eval      cmzmba = prmfir
052700021009     C***                endif
052800010201     C                   eval      cmznrr = *zeros
052900010201     C                   move      §ctrokvb      cmznrr
053000021018     C                   eval      cmzlba = vlrfl1
053100010201     C                   call(e)   'TIS711C'
053200010201     C                   parm                    dscmz
053300010201     C                   parm      *blanks       esito
053400010201     C                   if        %error
053500010201     C                             or cmzerr = '1'
053600010201     C                             or esito  = '1'
053700010201     C                   eval      wrkesito = '3'
053800010201     C                   endif
053900010205     C                   endif
054000990920     C*
054100000613     C                   ENDSR
054200000613     C***
054300081006
054400081006
054500081006
054600081006     C*----------------------------------------------------*
054700081006     C*  ROUTINE FINE ELABORAZIONE
054800081006     C*----------------------------------------------------*
054900081006     C     ENDELA        BEGSR
055000081006     C*
055100081006     C                   if        %open(tivin00r)
055200081006     C                   close     tivin00r
055300081006     C                   endif
055400081006     C                   if        %open(fivabwwr)
055500081006     C                   close     fivabwwr
055600081006     C                   endif
055700081006     C                   if        %open(fivadwwr)
055800081006     C                   close     fivadwwr
055900081006     C                   endif
056000081006     C*
056100081006     C* Effettuo la chiamata al CLLE preposto
056200081006     C                   call(e)   'TITVVDC'
056300081006     C                   parm                    parccm
056400081006     C                   parm                    parmbr
056500081006     C                   parm      '2'           paropz
056600081006     C*
056700081006     C                   if        %open(titv59ps)
056800081006     C                   except    findett
056900081006     C                   close     titv59ps
057000081006     C                   endif
057100081006     C*
057200081006     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
057300081006     C                   Z-ADD     150           LENGH            15 5
057400081006     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
057500081006     C                   PARM                    CMD(3)
057600081006     C                   PARM                    LENGH
057700081006     C*
057800081006     C                   ENDSR
057900070326
058000070326
058100081006
058200081006
058300081006     C     *pssr         BEGSR
058400081006     C*
058500081006     C                   if        %open(tivin00r)
058600081006     C                   close     tivin00r
058700081006     C                   endif
058800081006     C                   if        %open(fivabwwr)
058900081006     C                   close     fivabwwr
059000081006     C                   endif
059100081006     C                   if        %open(fivadwwr)
059200081006     C                   close     fivadwwr
059300081006     C                   endif
059400070326     C*
059500070326     C* Effettuo la chiamata al CLLE preposto
059600070326     C                   call(e)   'TITVVDC'
059700070326     C                   parm                    parccm
059800070326     C                   parm                    parmbr
059900070326     C                   parm      '2'           paropz
060000070326     C*
060100070326     C                   eval      wrkesito = '2'
060200070326     C*
060300070326     C                   ENDSR     '*CANCL'
060400070326     C***
060500070326
060600070326
060700990910
060800000613     C     *inzsr        BEGSR
060900990910     C*
061000990910     C     *entry        plist
061100990920     C                   parm                    tivlrds
061200990921     C                   parm      wrkesito      esito
061300000724     C                   parm                    prmlit
061400000710     C                   parm                    prmfir
061500000613     C*
061600000830     C* CALCOLA LA DATA CORRENTE
061700000830     C                   time                    wn14             14 0
061800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061900000830     C                   z-add     wn8           g08dat
062000000830     C                   z-add     *zeros        g08inv
062100000830     C                   movel     '0'           g08err
062200000830     C                   call      'XSRDA8'
062300000830     C                   parm                    wlbda8
062400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062500000830     C*
062600000613     C                   ENDSR
062700000613     C***
062800990908
062900030625     Otitv59p   E            riepilogo         2
063000990915     O                                              'Upload via Internet'
063100990915     O                                           +1 'Traduzione TIVIN00R -'
063200030625     O                                           +1 'FIVABWWR/FIVADWWR'
063300010201     O                                           +1 'Report testate bolle'
063400990915     O          E            riepilogo   2
063500990915     O                       wrkdata
063600990915     O                       wrkora              +1
063700990915     O                       procname            +1
063800990915     O          E            riepilogo   2
063900990915     O                                              'Cliente..................:'
064000990915     O                       VABCCM        z     +1
064100990915     O          E            riepilogo   2
064200990920     O                                              'Riferimento Strategi.....:'
064300990920     O                       vlrhdl              +1
064400990915     O          E            riepilogo   2
064500990915     O                                              'Giusti...................:'
064600010201     O                       §CTROKVB      2   +  1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Sbagliati e corretti.....:'
064900971022     O                       §CTRMO        2   +  1
065000990915     O          E            riepilogo   2
065100990915     O                                              'Sbagliati e scartati.....:'
065200971022     O                       §CTRNO        2   +  1
065300000613
065400030625     Otitv59ps  E            testdett          2
065500000613     O                                              'Upload via Internet'
065600000613     O                                           +1 'Traduzione TIVIN00R -'
065700030625     O                                           +1 'FIVABWWR/FIVADWWR'
065800010201     O                                           +1 'Report testate bolle'
065900000616     O          E            testdett    3
066000000613     O                                           +2 'N° rec'
066100000613     O                                           +3 'Anteprima contenuto'
066200000616     O          E            rigadett    2
066300000613     O                       rrnum               +2
066400000621     O                       recko               +3
066500000616     O          E            findett     2
066600000613     O                       wrkdata
066700000613     O                       wrkora              +1
066800000613     O                       procname            +1
066900000616     O          E            findett     2
067000000613     O                                              'Cliente..................:'
067100000613     O                       VABCCM        z     +1
067200000616     O          E            findett     2
067300000613     O                                              'Riferimento Strategi.....:'
067400000613     O                       vlrhdl              +1
067500000616     O          E            findett     2
067600000613     O                                              'Giusti...................:'
067700010201     O                       §CTROKVB      2   +  1
067800000616     O          E            findett     2
067900000613     O                                              'Sbagliati e corretti.....:'
068000000613     O                       §CTRMO        2   +  1
068100000616     O          E            findett     2
068200000613     O                                              'Sbagliati e scartati.....:'
068300000613     O                       §CTRNO        2   +  1
068400000512** CMD - COMANDI CL
068500030625OVRPRTF FILE(TITV59P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
068600030625OVRPRTF FILE(TITV59PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068700030625DLTOVR FILE(TITV59P TITV59PS) LVL(*)
068800000512
068900000512
