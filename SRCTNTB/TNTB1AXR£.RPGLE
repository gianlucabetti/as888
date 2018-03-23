000100121031       //==============================================================
000200121031       //?Gestione tabella "1AX" (Tipi incasso abilitati x LNA export) ?
000300121031       //==============================================================
000400121031
000500121031       //--------------------------------------------------------------
000600121031       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121031       //--------------------------------------------------------------
000800121031
000900121031     /*PRM dbgview(*source)
001000121031     /*END
001100121031
001200121031       //--------------------------------------------------------------
001300121031       //?Specifiche di controllo.                                     ?
001400121031       //--------------------------------------------------------------
001500121031
001600121031     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700121031     h dftactgrp(*no)
001800121031     h bnddir('TRUL')
001900121031
002000121031       //--------------------------------------------------------------
002100121031       //?Dichiarazione file.                                          ?
002200121031       //--------------------------------------------------------------
002300121031
002400121031       // -?Organigramma?
002500121031     fAZORG01L  if   e           k disk
002600121031
002700121031       // -?Tabella "1AX"?
002800121031     fTNTBE01L  Uf A e           k disk
002900121031
003000121031       // -?Video?
003100121031     fTNTB1AXD  cf   e             workstn
003200121031     f                                     sfile(TB1AXS01 : S01nrr)
003300121031     f                                     sfile(TB1AXS02 : S02nrr)
003400121031     f                                     indds(IndDspF)
003500121031     f                                     infds(InfDspF)
003600121031
003700121031       //--------------------------------------------------------------
003800121031       //?Definizione costanti.                                        ?
003900121031       //--------------------------------------------------------------
004000121031
004100121031       // -?Codice tabella in gestione?
004200121031     d c_Tab           c                   const('1AX')
004300121102
004400121102       // -?Numero di linee di arrivo export visualizzate in D01?
004500121102     d c_MaxLNAd01     c                   const(09)
004600121031
004700121102       // -?Numero di tipi incasso visualizzati in D01?
004800121031     d c_MaxTICd01     c                   const(09)
004900121031
005000121031       // -?Numero MAX di Tipi incasso c/assegno gestibili?
005100121031     d c_MaxTIC        c                   const(20)
005200121105
005300121105       // -?Tipo incasso c/assegno *BLANK (Assegno corriere/Contanti)?
005400121105       //  ?a video?
005500121105     d c_TIC_blank     c                   const('--')
005600121105
005700121105       // -?Costante per controllo "caratteri solo numerici"?
005800121105     d c_Digits        c                   const('0123456789')
005900121031
006000121031       // -?Tasti funzionali a video?
006100121031     d c_F01           c                   const(x'31')
006200121031     d c_F02           c                   const(x'32')
006300121031     d c_F03           c                   const(x'33')
006400121031     d c_F04           c                   const(x'34')
006500121031     d c_F05           c                   const(x'35')
006600121031     d c_F06           c                   const(x'36')
006700121031     d c_F07           c                   const(x'37')
006800121031     d c_F08           c                   const(x'38')
006900121031     d c_F09           c                   const(x'39')
007000121031     d c_F10           c                   const(x'3A')
007100121031     d c_F11           c                   const(x'3B')
007200121031     d c_F12           c                   const(x'3C')
007300121031     d c_F13           c                   const(x'B1')
007400121031     d c_F14           c                   const(x'B2')
007500121031     d c_F15           c                   const(x'B3')
007600121031     d c_F16           c                   const(x'B4')
007700121031     d c_F17           c                   const(x'B5')
007800121031     d c_F18           c                   const(x'B6')
007900121031     d c_F19           c                   const(x'B7')
008000121031     d c_F20           c                   const(x'B8')
008100121031     d c_F21           c                   const(x'B9')
008200121031     d c_F22           c                   const(x'BA')
008300121031     d c_F23           c                   const(x'BB')
008400121031     d c_F24           c                   const(x'BC')
008500121031     d c_Enter         c                   const(x'F1')
008600121031     d c_RollDown      c                   const(x'F4')
008700121031     d c_RollUp        c                   const(x'F5')
008800121031
008900121031       //--------------------------------------------------------------
009000121031       //?Definizione schiere.                                         ?
009100121031       //--------------------------------------------------------------
009200121031
009300121031       // -?Messaggi di errore?
009400121106     d $Msg            s             78    dim(12)  ctdata  perrcd( 1)
009500121031
009600121031       //--------------------------------------------------------------
009700121031       //?Definizione aree dati.                                       ?
009800121031       //--------------------------------------------------------------
009900121031
010000121031       // -?Dati utente?
010100121031     d §AzUte        e ds                  extname(AZUTE00F)
010200121031     d                                     dtaara
010300121031     d §DatiUte      e ds                  extname(dDatiUte)
010400121031     d                                     dtaara
010500121031
010600121031       //--------------------------------------------------------------
010700121031       //?Definizione strutture dati.                                  ?
010800121031       //--------------------------------------------------------------
010900121031
011000121031       // -?Status ds?
011100121031     d Status         sds
011200121031     d   SDSpgm          *proc
011300121031     d*//SDSprm          *parms
011400121031
011500121031       // -?InfDS?
011600121031     d InfDspF         ds
011700121031     d   dsp_aid             369    369a
011800121031     d   sfl_rrn             376    377i 0
011900121031     d   min_nrr             378    379i 0
012000121031     d   num_rcds            380    381i 0
012100121031
012200121031       // -?Indicatori su DspF?
012300121031     d IndDspF         ds                  inz
012400121031         // -?Abilitazione tasti funzionali?
012500121102     d   F3Attivo                      n   overlay(IndDspF : 03)
012600121031     d   F6Attivo                      n   overlay(IndDspF : 06)
012700121102     d   F8Attivo                      n   overlay(IndDspF : 08)
012800121102     d   F9Attivo                      n   overlay(IndDspF : 09)
012900121106     d   F12Attivo                     n   overlay(IndDspF : 12)
013000121106     d   F17Attivo                     n   overlay(IndDspF : 17)
013100121031         // -?Emissione messaggio di errore?
013200121031     d   ErrMessage                    n   overlay(IndDspF : 28)
013300121031         // -?Indicatori di gestione del subfile?
013400121031     d   SflDsp_N                      n   overlay(IndDspF : 30)
013500121031     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
013600121031     d   SflNxtChg                     n   overlay(IndDspF : 32)
013700121031     d   SflEnd                        n   overlay(IndDspF : 33)
013800121031     d   Sfl2Dsp_N                     n   overlay(IndDspF : 34)
013900121031     d   Sfl2DspCtl_N                  n   overlay(IndDspF : 35)
014000121031     d   Sfl2NxtChg                    n   overlay(IndDspF : 36)
014100121031     d   Sfl2End                       n   overlay(IndDspF : 37)
014200121031         // -?Indicatori per Attibuti di visualizzazione?
014300121031     d   NoRecSfl1                     n   overlay(IndDspF : 40)
014400121031     d   Ord_x_Des                     n   overlay(IndDspF : 41)
014500121031     d   ProtectKEY                    n   overlay(IndDspF : 42)
014600121106     d*//ProtectSfl2                   n   overlay(IndDspF : 43)
014700121031         // -?Posizionamento cursore & segnalazione errore?
014800121031     d   PosCurOPZ                     n   overlay(IndDspF : 50)
014900121102     d   PosCurLNA                     n   overlay(IndDspF : 51) dim(9)
015000121102     d*//PosCurLNA1                    n   overlay(IndDspF : 51)
015100121102     d*//PosCurLNA2                    n   overlay(IndDspF : 52)
015200121102     d*//PosCurLNA3                    n   overlay(IndDspF : 53)
015300121102     d*//PosCurLNA4                    n   overlay(IndDspF : 54)
015400121102     d*//PosCurLNA5                    n   overlay(IndDspF : 55)
015500121102     d*//PosCurLNA6                    n   overlay(IndDspF : 56)
015600121102     d*//PosCurLNA7                    n   overlay(IndDspF : 57)
015700121102     d*//PosCurLNA8                    n   overlay(IndDspF : 58)
015800121102     d*//PosCurLNA9                    n   overlay(IndDspF : 59)
015900121102     d   PosCurTIC                     n   overlay(IndDspF : 60) dim(9)
016000121102     d*//PosCurTIC1                    n   overlay(IndDspF : 60)
016100121102     d*//PosCurTIC2                    n   overlay(IndDspF : 61)
016200121102     d*//PosCurTIC3                    n   overlay(IndDspF : 62)
016300121102     d*//PosCurTIC4                    n   overlay(IndDspF : 63)
016400121102     d*//PosCurTIC5                    n   overlay(IndDspF : 64)
016500121102     d*//PosCurTIC6                    n   overlay(IndDspF : 65)
016600121102     d*//PosCurTIC7                    n   overlay(IndDspF : 66)
016700121102     d*//PosCurTIC8                    n   overlay(IndDspF : 67)
016800121102     d*//PosCurTIC9                    n   overlay(IndDspF : 68)
016900121102     d   PosCurDPD                     n   overlay(IndDspF : 69)
017000121102     d   PosCurTICs2                   n   overlay(IndDspF : 70)
017100121105     d   PosCurLNAc2                   n   overlay(IndDspF : 71)
017200121031         //   -?Riemissione videata?
017300121031     d   ErrGenerico                   n   overlay(IndDspF : 99)
017400121031
017500121031       // -?LNA export per Tipi incasso c/assegno a video in D01 (09)?
017600121102     d V1Clna_ds       ds                  inz
017700121031     d   V1Clna1                           inz
017800121031     d   V1Clna2                           inz
017900121031     d   V1Clna3                           inz
018000121031     d   V1Clna4                           inz
018100121031     d   V1Clna5                           inz
018200121031     d   V1Clna6                           inz
018300121031     d   V1Clna7                           inz
018400121031     d   V1Clna8                           inz
018500121031     d   V1Clna9                           inz
018600121102     d     sch_D01lna          1     27  0 inz  dim(c_MaxLNAd01)
018700121102     d V1Dlna_ds       ds                  inz
018800121102     d   V1Dlna1                           inz
018900121102     d   V1Dlna2                           inz
019000121102     d   V1Dlna3                           inz
019100121102     d   V1Dlna4                           inz
019200121102     d   V1Dlna5                           inz
019300121102     d   V1Dlna6                           inz
019400121102     d   V1Dlna7                           inz
019500121102     d   V1Dlna8                           inz
019600121102     d   V1Dlna9                           inz
019700121102     d     sch_D01dla          1    180    inz  dim(c_MaxLNAd01)
019800121031
019900121031       // -?Tipi incasso c/assegno per LNA export a video in D01 (09)?
020000121102     d V1Ctic_ds       ds                  inz
020100121031     d   V1Ctic1                           inz
020200121031     d   V1Ctic2                           inz
020300121031     d   V1Ctic3                           inz
020400121031     d   V1Ctic4                           inz
020500121031     d   V1Ctic5                           inz
020600121031     d   V1Ctic6                           inz
020700121031     d   V1Ctic7                           inz
020800121031     d   V1Ctic8                           inz
020900121031     d   V1Ctic9                           inz
021000121031     d     sch_D01tic          1     18    inz  dim(c_MaxTICd01)
021100121102     d V1Dtic_ds       ds                  inz
021200121102     d   V1Dtic1                           inz
021300121102     d   V1Dtic2                           inz
021400121102     d   V1Dtic3                           inz
021500121102     d   V1Dtic4                           inz
021600121102     d   V1Dtic5                           inz
021700121102     d   V1Dtic6                           inz
021800121102     d   V1Dtic7                           inz
021900121102     d   V1Dtic8                           inz
022000121102     d   V1Dtic9                           inz
022100121102     d     sch_D01dti          1    225    inz  dim(c_MaxTICd01)
022200121031
022300121031       // -?Tipi incasso c/assegno per LNA export in tab. "1AX"?
022400121031       //  ?(di singola linea di arrivo export) - max 20?
022500121031     d sch_TIC_ds      ds            40    inz
022600121031     d   sch_TIC               1     40    inz  dim(c_MaxTIC)
022700121105     d sch_TICd_ds     ds           500    inz
022800121105     d   sch_TICd              1    500    inz  dim(c_MaxTIC)
022900121031     d sch_TIC_ann_ds  ds            40    inz
023000121031     d   sch_TIC_ann           1     40    inz  dim(c_MaxTIC)
023100121031
023200121031       // -?Parametri ricevuti?
023300121031     d KPJBA         e ds
023400121031
023500121031       // -?Dati record principale della tabella?
023600121031     d TNTBEds       e ds                  inz  extname(TNTBE00F)
023700121031     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
023800121031     d                                          prefix(TBX:3)
023900121031
024000121031       // -?Tabelle usate:?
024100121031       // ·?"1A" = Codici tipo incasso c/assegno?
024200121031     d ds1A          e ds                  inz
024300121031       // ·?"1AX" = Tipi incasso abilitati per linea di arrivo export?
024400121031     d*// d1AX          e ds                  inz  qualified
024500121102
024600121102       // -?143ª descrizione dell'organigramma?
024700121102     d Og143         e ds                  inz  qualified
024800121031
024900121031       //--------------------------------------------------------------
025000121031       //?Definizione variabili globali.                               ?
025100121031       //--------------------------------------------------------------
025200121031
025300121031       // -?Flags booleani?
025400121031     d $Fine           s               n   inz
025500121031     d $EoF            s               n   inz
025600121102     d $RecOk          s               n   inz
025700121031     d $Immissione     s               n   inz
025800121031     d $InzD01         s               n   inz(*on)
025900121031     d $InzS01         s               n   inz(*on)
026000121031     d $InzS02         s               n   inz(*on)
026100121031     d $InzW01         s               n   inz(*on)
026200121031
026300121031       // -?Indici di schiera / Contatori?
026400121031     d xx              s              3  0 inz
026500121031     d yy              s              3  0 inz
026600121031
026700121031       // -?Variabili per la gestione del video?
026800121106     d $Video          s              2    inz('S1')
026900121031     d S01nrr          s                   like(C1RcdNbr) inz
027000121031     d S02nrr          s                   like(C2RcdNbr) inz
027100121031     d SavS01csr       s                   like(C1RcdNbr) inz
027200121102
027300121102       // -?Variabili per la gestione del posizionamento nel subfile?
027400121102     d SavC1Clna       s                   like(C1Clna)   inz(*hival)
027500121102     d SavC1Dlna       s                   like(C1Dlna)   inz(*hival)
027600121031
027700121031       // -?Variabili di comodo?
027800121031     d wData_EUR       s               d   datfmt(*eur)  inz(*loval)
027900121031
028000121031       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
028100121031
028200121031       //--------------------------------------------------------------
028300121031       // -?Constants?
028400121031       //--------------------------------------------------------------
028500121031       // -?MaxKey - Maximum number of key fields allowed by this program?
028600121031     d MaxKey          c                   const(2)
028700121031       // -?Sort order:?
028800121031       //  ?1 = Sort in an ascending sequence?
028900121031       //  ?2 = Sort in a descending sequence?
029000121031     d Ascendente      c                   const(1)
029100121031     d Discendente     c                   const(2)
029200121031       // -?Key data type:?
029300121031       //  ? 0 = Signed binary?
029400121031       //  ? 2 = Signed zoned decimal?
029500121031       //  ? 3 = Signed packed decimal?
029600121031       //  ? 6 = Character with no national language sort sequence applied,?
029700121031       //  ?     if specified?
029800121031       //  ? 7 = Unsigned packed decimal?
029900121031       //  ?     All numbers will have the sign forced positive ('F'X).?
030000121031       //  ? 8 = Unsigned zoned decimal?
030100121031       //  ?     All numbers will have the sign forced positive ('F'X).?
030200121031       //  ? 9 = Unsigned binary?
030300121031       //  ?14 = Date in form of DD/MM/YY?
030400121031       //  ?15 = Date in form of DD.MM.YYYY?
030500121031     d Numero          c                   const(2)
030600121031     d Carattere       c                   const(6)
030700121031       //
030800121031     d Put             c                   const(1)
030900121031     d EndPut          c                   const(2)
031000121031     d Get             c                   const(3)
031100121031
031200121031       //--------------------------------------------------------------
031300121031       // -?Data Structures?
031400121031       //  ?SflRcd     - The entire subfile record to pass to the sort?
031500121031       //  ?QLGSCB     - The sort request block for the QLGSORT API?
031600121031       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
031700121031       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
031800121031       //  ?QUSEC      - Error structure for the QLGSORT API?
031900121031       //--------------------------------------------------------------
032000121031     d SflRcd          ds                  inz
032100121102     d   S1Clna                            inz
032200121102     d   S1Dlna                            inz
032300121102     d   S1Ctic1                           inz
032400121102     d   S1Ctic2                           inz
032500121102     d   S1Ctic3                           inz
032600121102     d   S1Ctic4                           inz
032700121102     d   S1Ctic5                           inz
032800121102     d   S1Ctic6                           inz
032900121102     d   S1Ctic7                           inz
033000121102     d   S1Ctic8                           inz
033100121102     d   S1Ctic9                           inz
033200121102     d   S1CticA                           inz
033300121102     d   S1CticB                           inz
033400121102     d   S1CticC                           inz
033500121102     d   S1CticD                           inz
033600121102     d   S1CticE                           inz
033700121102     d   S1CticF                           inz
033800121102     d   S1CticG                           inz
033900121102     d   S1CticH                           inz
034000121102     d   S1CticI                           inz
034100121102     d   S1CticJ                           inz
034200121102     d   S1CticK                           inz
034300121105     d     S1Ctic_ds          24     63
034400121102     d   S1Copz                            inz
034500121102     d   Selected                     1a   inz
034600121031      /copy QSYSINC/QRPGLESRC,QLGSORT
034700121031     d QLGKL                         16    dim(MaxKey)
034800121102     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
034900121102     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
035000121102     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
035100121102     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
035200121031      /copy QSYSINC/QRPGLESRC,QLGSRTIO
035300121031      /copy QSYSINC/QRPGLESRC,QUSEC
035400121031
035500121031       //--------------------------------------------------------------
035600121031       // -?Standalone fields?
035700121031       //  ?Nrr        - Relative record number for subfile?
035800121031       //  ?SizeList   - Size of list?
035900121031       //  ?ReturnSize - Size of list returned by sort API?
036000121031       //--------------------------------------------------------------
036100121031     d NotUsed         s             16a   inz
036200121031     d ReturnSize      s              9b 0 inz
036300121031     d SizeList        s              9b 0 inz
036400121031     d RrnLast         s              5i 0 inz
036500121031     d Nrr             s              5i 0 inz
036600121031
036700121031       //--------------------------------------------------------------
036800121031       //?Definizione prototipi procedure.                             ?
036900121031       //--------------------------------------------------------------
037000121031
037100121031       // -?Reperimento dati utente?
037200121031     d TIBS34ds      e ds                  inz
037300121031      /copy gaitrasrc/srcProtoPR,TIBS34R
037400121105
037500121105       // -?Ricerca tabella TABEL00F?
037600121105     d idElemento      s              8    inz
037700121105     d tastoFunzionaleUscita...
037800121105     d                 s              1    inz
037900121105      /copy gaitrasrc/srcProtoPR,TRUL19R
038000121031
038100121031       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
038200121031     d c_TABEL         c                   const('1')
038300121031       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
038400121031     d c_TNTBE         c                   const('2')
038500121031       // -?Reperimento dati tabelle?
038600121031      /copy gaitrasrc/srcProtoPR,TRULTAB
038700121031
038800121031       // -?Ordinamento subfile?
038900121031      /copy gaitrasrc/srcProtoPr,QLGSRTIO
039000121031
039100121031       //--------------------------------------------------------------
039200121031       //?Definizione key-list.                                        ?
039300121031       //--------------------------------------------------------------
039400121031
039500121031       // -?File TNTBE01L?
039600121031     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
039700121031     d                                     prefix(k_)   inz
039800121031
039900121031       // -?File AZORG01L?
040000121031     d k01azorg01    e ds                  extname(AZORG01L : *key)
040100121031     d                                     prefix(k_)   inz
040200121031
040300121031       //--------------------------------------------------------------
040400121031       //?Riepilogo indicatori utilizzati.                             ?
040500121031       //--------------------------------------------------------------
040600121031
040700121031
040800121031       //--------------------------------------------------------------
040900121031       //?M A I N - L I N E                                            ?
041000121031       //--------------------------------------------------------------
041100121031
041200121031     c     *Entry        plist
041300121031     c                   parm                    KPJBA
041400121031
041500121031      /free
041600121031
041700121031       // -?Operazioni iniziali?
041800121031       exsr  sr_RoutInz;
041900121031
042000121031       // -?Ciclo di gestione del file video?
042100121031       DoW  $Fine = *off;
042200121031
042300121031         select;
042400121031
042500121031           // -?Gestione videata S1 (subfile)?
042600121031           when  $Video = 'S1';
042700121031             exsr  sr_GesS01;
042800121031
042900121031           // -?Gestione videata S2 (subfile)?
043000121031           //  ?(da qui solo per "immissione")?
043100121031           when  $Video = 'S2';
043200121031             exsr  sr_GesS02;
043300121102
043400121102           // -?Gestione videata D1 (parzializzazioni)?
043500121102           when  $Video = 'D1';
043600121102             exsr  sr_GesD01;
043700121031
043800121031           // -?Richiesta trasmissione dati?
043900121031           //  ?(NON da qui)?
044000121031           //when  $Video = 'W1';
044100121031           //  exsr  sr_GesW01;
044200121031
044300121031           // -?? ? ??
044400121031           other;
044500121031             $Fine = *on;
044600121031
044700121031         endsl;
044800121031
044900121031       EndDo;
045000121031
045100121031       // -?Operazioni finali?
045200121031       exsr  sr_RoutEnd;
045300121031
045400121031       //--------------------------------------------------------------
045500121031       //?Operazioni iniziali.                                         ?
045600121031       //--------------------------------------------------------------
045700121031       BEGSR  sr_RoutInz;
045800121031
045900121031         // -?Impostazione chiusura?
046000121031         *inLR = *on;
046100121031
046200121031         // -?Reperimento dati job?
046300121031         exsr  sr_DatiJob;
046400121031
046500121031         // -?Impostazione nome programma a video?
046600121031         V1Tpgm = SDSpgm;
046700121031
046800121031         // -?Aggancio dati generali della tabella in esame?
046900121031         clear  k_TBEcod;
047000121031         k_TBEke1 = *zero;
047100121031         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
047200121031                = c_Tab;
047300121031         clear  k_TBEke2;
047400121031         clear  k_TBElin;
047500121031         k_TBEsif = KNSIF;
047600121031         chain  %kds(k05tntbe01)  TNTBE000;
047700121031         if  not %found(TNTBE01L);
047800121031           clear  k_TBEsif;
047900121031           chain  %kds(k05tntbe01)  TNTBE000;
048000121031         endif;
048100121031         if  %found(TNTBE01L);
048200121031           xTNTBEds = TNTBEds;
048300121031         else;
048400121031           clear  xTNTBEds;
048500121031         endif;
048600121031
048700121031         // -?Pulizia / impostazione iniziale dei campi chiave?
048800121031         clear  k05tntbe01;
048900121031         k_TBEcod = c_Tab;
049000121106
049100121106         // -?Impostazione videata iniziale?
049200121106         $Video = 'S1';
049300121106         reset  $InzS01;
049400121106         reset  $InzS02;
049500121106         reset  $InzD01;
049600121106         exsr  sr_InzD01;
049700121106         $InzD01 = *off;
049800121031
049900121031       ENDSR;
050000121031
050100121031       //--------------------------------------------------------------
050200121031       //?Reperimento Dati del job (Utente/Operativi).                 ?
050300121031       //--------------------------------------------------------------
050400121031       BEGSR  sr_DatiJob;
050500121031
050600121031         in(e) §AzUte;
050700121031         if NOT %error;
050800121031           in(e) §DatiUte;
050900121031         endif;
051000121031         if %error or RSut = *blank;
051100121031           tibs34r ( tibs34ds );
051200121031           in §AzUte;
051300121031           in §DatiUte;
051400121031         endif;
051500121031
051600121031       ENDSR;
051700121102
051800121102       //--------------------------------------------------------------
051900121102       //?Gestione videata D01.                                        ?
052000121102       //--------------------------------------------------------------
052100121102       BEGSR  sr_GesD01;
052200121102
052300121102         // -?Inizializzazione videata?
052400121102         if  $InzD01 = *on;
052500121102           exsr  sr_InzD01;
052600121102           $InzD01 = *off;
052700121102         endif;
052800121105
052900121105         // -?Abilitazione tasti funzionali?
053000121105         F3Attivo  = *on;
053100121106         F12Attivo = *on;
053200121102
053300121102         // -?Emissione Testata?
053400121102         write  TB1AXT01;
053500121102
053600121102         // -?Emissione videata?
053700121102         exfmt  TB1AXD01;
053800121102
053900121102         reset  ErrMessage;
054000121102         reset  ErrGenerico;
054100121102         clear  V1Dmsg;
054200121102
054300121102         select;
054400121102
054500121102           // -?F3=Fine?
054600121102           when  dsp_aid = c_F03;
054700121102             $Fine = *on;
054800121106
054900121106           // -?F12=Ritorno?
055000121106           when  dsp_aid = c_F12;
055100121106             $Video = 'S1';
055200121102
055300121102           // -?Invio?
055400121102           other;
055500121102             exsr  sr_CtrD01;
055600121102             if  not  ErrGenerico;
055700121102               $Video = 'S1';
055800121102               reset  $InzS01;
055900121102             endif;
056000121102
056100121102         endsl;
056200121102
056300121102       ENDSR;
056400121102
056500121102       //--------------------------------------------------------------
056600121102       //?Inizializzazione videata D01.                                ?
056700121102       //--------------------------------------------------------------
056800121102       BEGSR  sr_InzD01;
056900121102
057000121102         // -?Pulizia videata?
057100121102         clear  TB1AXD01;
057200121102
057300121102       ENDSR;
057400121102
057500121102       //--------------------------------------------------------------
057600121102       //?Controllo dati immessi nella videata D01.                    ?
057700121102       //--------------------------------------------------------------
057800121102       BEGSR  sr_CtrD01;
057900121102
058000121102         // -?Spegnimento degli indicatori di errore?
058100121102         %subst(IndDspF : 50) = *off;
058200121102
058300121102         // -?Esclusione troppe linee di arrivo?
058400121102         if  V1Cdpd = 'S'  and  V1Ceex = 'S'  and  V1Cfed = 'S';
058500121105           V1Dmsg = $Msg(09);
058600121102           PosCurDPD   = *on;
058700121102           ErrMessage  = *on;
058800121102           ErrGenerico = *on;
058900121102           leavesr;
059000121102         endif;
059100121102
059200121102         // -?Pulizia decodifiche LNA x export & Tipi incasso c/assegno?
059300121102         clear  V1Dlna_ds;
059400121102         clear  V1Dtic_ds;
059500121102
059600121102         // -?Controllo Linee di arrivo per export?
059700121106         IF  V1Clna_ds <> *blank  and  V1Clna_ds <> *zero;
059800121105           For  xx = 1  To  %elem(sch_D01lna);
059900121105             if  sch_D01lna(xx) <> *zero;
060000121105               // -?Inserita più di una volta?
060100121105               if  xx  > 1;
060200121105                 yy = %lookup(sch_D01lna(xx) : sch_D01lna);
060300121105                 if  yy < xx;
060400121105                   sch_D01dla(yy) = sch_D01dla(xx);
060500121105                   V1Dmsg = $Msg(10);
060600121105                   PosCurLNA(xx) = *on;
060700121105                   ErrMessage  = *on;
060800121105                   ErrGenerico = *on;
060900121105                   leavesr;
061000121105                 endif;
061100121105               endif;
061200121105               // -?Errata?
061300121106               clear  Og143;
061400121105               k_ORGfil = sch_D01lna(xx);
061500121105               chain  %kds(k01azorg01)  AZORG;
061600121106               if  Not %found(AZORG01L)  or  ORGfva <> *blank;
061700121105                 sch_D01dla(xx) = *all'? ';
061800121105                 V1Dmsg = $Msg(11);
061900121105                 PosCurLNA(xx) = *on;
062000121105                 ErrMessage  = *on;
062100121105                 ErrGenerico = *on;
062200121105                 leavesr;
062300121105               endif;
062400121105               sch_D01dla(xx) = ORGdes;
062500121106               Og143 = ORGde3;
062600121106               // -?Ammesse solo filiali per export?
062700121106               if  Og143.§OGntw <> 'DPD'  and
062800121106                   Og143.§OGntw <> 'EEX'  and
062900121106                   Og143.§OGntw <> 'FED';
063000121106                 V1Dmsg = $Msg(11);
063100121106                 PosCurLNA(xx) = *on;
063200121106                 ErrMessage  = *on;
063300121106                 ErrGenerico = *on;
063400121106                 leavesr;
063500121106               endif;
063600121105               // -?Già esclusa come DPD/EEX/FED?
063700121105               if  (Og143.§OGntw = 'DPD'  and  V1Cdpd = 'S')  or
063800121105                   (Og143.§OGntw = 'EEX'  and  V1Ceex = 'S')  or
063900121105                   (Og143.§OGntw = 'FED'  and  V1Cfed = 'S');
064000121105                 select;
064100121105                   when  Og143.§OGntw = 'DPD'  and  V1Cdpd = 'S';
064200121105                     V1Dmsg = %trimr($Msg(12)) + ' DPD';
064300121105                   when  Og143.§OGntw = 'EEX'  and  V1Ceex = 'S';
064400121105                     V1Dmsg = %trimr($Msg(12)) + ' EuroExpress';
064500121105                   when  Og143.§OGntw = 'FED'  and  V1Cfed = 'S';
064600121105                     V1Dmsg = %trimr($Msg(12)) + ' FedEx';
064700121105                 endsl;
064800121105                 PosCurLNA(xx) = *on;
064900121105                 ErrMessage  = *on;
065000121105                 ErrGenerico = *on;
065100121105                 leavesr;
065200121105               endif;
065300121105             endif;
065400121105           EndFor;
065500121105         ENDIF;
065600121102
065700121102         // -?Controllo Tipi incasso c/assegno?
065800121105         IF  V1Ctic_ds <> *blank;
065900121105           For  xx = 1  To  %elem(sch_D01tic);
066000121105             if  sch_D01tic(xx) <> *blank;
066100121106               // -?Ricerca?
066200121106               if  %scan('?' : sch_D01tic(xx)) > *zero;
066300121106                 clear  sch_D01tic(xx);
066400121106                 PosCurTIC(xx) = *on;
066500121106                 ErrGenerico = *on;
066600121106                 TABEL_Ricerca( '1A' :
066700121106                                *blank :
066800121106                                idElemento :
066900121106                                tastoFunzionaleUscita );
067000121106                 if  tastoFunzionaleUscita = *blank;
067100121106                   if  idElemento = *blank;
067200121106                     sch_D01tic(xx) = c_TIC_blank;
067300121106                   else;
067400121106                     sch_D01tic(xx) = idElemento;
067500121106                   endif;
067600121106                 else;
067700121106                   leavesr;
067800121106                 endif;
067900121106               endif;
068000121105               // -?Inserito più di una volta?
068100121105               if  xx  > 1;
068200121105                 yy = %lookup(sch_D01tic(xx) : sch_D01tic);
068300121105                 if  yy < xx;
068400121105                   sch_D01dti(yy) = sch_D01dti(xx);
068500121105                   V1Dmsg = $Msg(05);
068600121105                   PosCurTIC(xx) = *on;
068700121105                   ErrMessage  = *on;
068800121105                   ErrGenerico = *on;
068900121105                   leavesr;
069000121105                 endif;
069100121105               endif;
069200121105               // -?Errato?
069300121105               if  sch_D01tic(xx) = c_TIC_blank;
069400121105                 clear  ds_TNTBE.TBEke1;
069500121105               else;
069600121105                 ds_TNTBE.TBEke1 = sch_D01tic(xx);
069700121105               endif;
069800121105               clear  ds_TNTBE.TBEke2;
069900121105               if  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
070000121105                                *omit : *omit : *omit :
070100121105                                *omit : *omit :
070200121105                                *omit : *omit : *omit : *omit :
070300121105                                *omit : *omit : *omit : *omit :
070400121105                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
070500121105                              <> *zero   OR
070600121105                              ds_TNTBE.TBEatb <> *blank;
070700121105                 sch_D01dti(xx) = *all'? ';
070800121105                 V1Dmsg = $Msg(06);
070900121105                 PosCurTIC(xx) = *on;
071000121105                 ErrMessage  = *on;
071100121105                 ErrGenerico = *on;
071200121105                 leavesr;
071300121105               endif;
071400121105               ds1A   = ds_TNTBE.TBEuni;
071500121105               sch_D01dti(xx) = §1Ades;
071600121105             endif;
071700121105           EndFor;
071800121105         ENDIF;
071900121102
072000121102       ENDSR;
072100121031
072200121031       //--------------------------------------------------------------
072300121031       //?Gestione subfile S01.                                        ?
072400121031       //--------------------------------------------------------------
072500121031       BEGSR  sr_GesS01;
072600121031
072700121031         // -?Inizializzazione videata?
072800121031         if  $InzS01 = *on;
072900121031           exsr  sr_InzS01;
073000121031           $InzS01 = *off;
073100121031         endif;
073200121106
073300121106         // -?Abilitazione tasti funzionali?
073400121106         F3Attivo  = *on;
073500121106         F6Attivo  = *on;
073600121106         F8Attivo  = *on;
073700121106         F9Attivo  = (S01nrr > *zero);
073800121106         F12Attivo = *off;
073900121106         F17Attivo = *on;
074000121031
074100121031         // -?Emissione Testata & Piede?
074200121031         write  tb1AXt01;
074300121031         write  tb1AXp01;
074400121031
074500121031         // -?Posizionamento cursore?
074600121031         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
074700121031         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
074800121031         //  ?si visualizza la pagina che vedeva l'utente quando ha?
074900121031         //  ?premuto l'ultimo tasto.?
075000121031         if  C1CsrRrn > *zero;
075100121031           C1RcdNbr = C1CsrRrn;
075200121031         else;
075300121031           C1RcdNbr = 1;
075400121031           write  tb1AXs00;            //?(no rec.)?
075500121031         endif;
075600121031
075700121031         // -?Emissione videata?
075800121031         exfmt  tb1AXc01;
075900121031
076000121031         reset  ErrGenerico;
076100121031         reset  ErrMessage;
076200121031         clear  V1Dmsg;
076300121031
076400121031         Select;
076500121031
076600121031           // -?F3=Fine?
076700121031           When  dsp_aid = c_F03;
076800121031             $Fine = *on;
076900121031
077000121031           // -?F5=Rivisualizza?
077100121031           When  dsp_aid = c_F05;
077200121031             $InzS01 = *on;
077300121105
077400121105           // -?F6=Immissione?
077500121105           When  dsp_aid = c_F06;
077600121105             $Video  = 'S2';
077700121105             $InzS02 = *on;
077800121105             //DoW  $Video = 'S2'  and  Not $Fine;
077900121105             //  exsr  sr_GesS02;
078000121105             //EndDo;
078100121031
078200121031           // -?F8=Ordinamento?
078300121031           when  dsp_aid = c_F08;
078400121031             exsr  sr_F08S01;
078500121031
078600121031           // -?F12=Ritorno?
078700121106           //when  dsp_aid = c_F12;
078800121106
078900121106           // -?F17=Parzializzazioni?
079000121106           when  dsp_aid = c_F17;
079100121031             eval  $Video = 'D1';
079200121031
079300121031           // -?Invio?
079400121031           Other;
079500121031             // -?Gestione opzioni?
079600121102             if  S01nrr > *zero;
079700121102               exsr  sr_OpzS01;
079800121102               if  ErrGenerico;
079900121102                 leavesr;
080000121102               endif;
080100121102             endif;
080200121031             // -?Richiesto posizionamento?
080300121031             if  (Not Ord_x_Des   and   C1Clna <> SavC1Clna)   OR
080400121031                 (Ord_x_Des       and   C1Dlna <> SavC1Dlna);
080500121031               $InzS01 = *on;
080600121031             endif;
080700121031
080800121031         endsl;
080900121031
081000121031       ENDSR;
081100121031
081200121031       //--------------------------------------------------------------
081300121031       //?Inizializzazione subfile S01.                                ?
081400121031       //--------------------------------------------------------------
081500121031       BEGSR  sr_InzS01;
081600121031
081700121031         // -?Spegnimento degli indicatori di errore?
081800121031         %subst(IndDspF : 50) = *off;
081900121031
082000121031         // -?Pulizia subfile control?
082100121031         if  Not Ord_x_Des;
082200121031           clear  C1Dlna;
082300121031           reset  SavC1Dlna;
082400121031         else;
082500121031           clear  C1Clna;
082600121031           reset  SavC1Clna;
082700121031         endif;
082800121031
082900121031         // -?Pulizia subfile?
083000121031         SflDsp_N    = *on;
083100121031         SflDspCtl_N = *on;
083200121031         write  tb1AXc01;
083300121031         SflDspCtl_N = *off;
083400121031         SflEnd      = *off;
083500121031
083600121031         clear  C1RcdNbr;
083700121031         clear  C1CsrRrn;
083800121031         clear  S01nrr;
083900121031         clear  V1Dmsg;
084000121031         ErrMessage  = *off;
084100121031         ErrGenerico = *off;
084200121031
084300121031         // -?Caricamento completo dei dati nel subfile?
084400121031         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
084500121031         //  ?l'eventuale ordinamento)?
084600121105         clear  k05tntbe01;
084700121105         k_TBEcod = c_Tab;
084800121105         setll  %kds( k05tntbe01 : 1 )  TNTBE000;
084900121105         reade(N)  %kds( k05tntbe01 : 1 )  TNTBE000;
085000121031         $EoF = (%eof(TNTBE01L));
085100121031         exsr  sr_CaricaS01;
085200121031
085300121031         // -?Impaginazione subfile?
085400121031         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
085500121031         if S01nrr > *zero;
085600121031           C1RcdNbr  = 1;
085700121031           C1CsrRrn  = 1;
085800121031         else;
085900121031           clear C1RcdNbr;
086000121031         endif;
086100121031
086200121031         // -?Ordinamento subfile (se premuto F8)?
086300121031         if  Ord_x_Des   and   S01nrr >  *zero;
086400121031           exsr  sr_SortSfl;
086500121031         endif;
086600121031
086700121031       ENDSR;
086800121031
086900121031       //--------------------------------------------------------------
087000121031       //?Caricamento completo del subfile S01                         ?
087100121031       //--------------------------------------------------------------
087200121031       BEGSR  sr_CaricaS01;
087300121031
087400121031         clear  S01nrr;
087500121031         SflNxtChg = *off;
087600121031
087700121031         // -?Ciclo di caricamento dati da tab. "1AX"?
087800121031         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
087900121031         //  ?l'eventuale ordinamento)?
088000121105         clear  k_ORGfil;
088100121031         DOW  Not $EoF;
088200121031
088300121105           // -?Caricamento tipi incasso c/assegno (già inseriti)?
088400121105           //  ?della LNA export in elaborazione (TBEKE1)?
088500121031           k_TBEke1 = TBEke1;
088600121031           clear  xx;
088700121031           clear  yy;
088800121031           clear  sch_TIC_ds;
088900121031           clear  sch_TIC_ann_ds;
089000121031           clear  tb1AXs01;
089100121031           S1Clna = %subst( TBEke1 : 1 : %len(S1Clna) );
089200121031
089300121102           DoW  Not %eof(TNTBE01L)  and  TBEke1 = S1Clna;
089400121031
089500121031             // -?Selezione singolo record file TNTBE00F?
089600121031             exsr  sr_SelezRec;
089700121031
089800121031             // -?Registrazione singolo record nel subfile?
089900121031             If  $RecOK;
090000121031
090100121031               // ·?Impostazione Tipo incasso c/assegno?
090200121031               xx += 1;
090300121031               if  TBEke2 = *blank;
090400121105                 sch_TIC(xx) = c_TIC_blank;
090500121031               else;
090600121102                 sch_TIC(xx) = %subst( TBEke2 : 1 : %len(S1Ctic1) );
090700121031               endif;
090800121031
090900121031             EndIf;
091000121031
091100121105             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
091200121031
091300121031           EndDo;
091400121105
091500121105           // *?Scrittura del record SE reperito almeno un?
091600121105           //  ?tipo incasso c/assegno NON annullato?
091700121105           If  xx > *zero;
091800121031
091900121105             // -?Decodifica LNA export?
092000121105             if  %check( c_Digits : S1Clna ) = *zero;
092100121105               k_ORGfil = %int( S1Clna );
092200121105               chain  %kds(k01azorg01)  AZORG;
092300121105               if  %found(AZORG01L)  and  ORGfva = *blank;
092400121105                 S1Dlna = ORGdes;
092500121105               else;
092600121105                 S1Dlna = *all'? ';
092700121105               endif;
092800121105             endif;
092900121105
093000121105             // -?Scrittura record nel subfile?
093100121105             if  sch_TIC_ds <> *blank;
093200121105               S1Ctic_ds = sch_TIC_ds;
093300121105               S01nrr += 1;
093400121105               write  tb1AXs01;
093500121105             endif;
093600121105
093700121105           EndIf;
093800121031
093900121031           // -?Lettura record della LNA export successiva?
094000121105           setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
094100121105           reade(N)  %kds( k05tntbe01 : 1 )  TNTBE000;
094200121031           $EoF = (%eof(TNTBE01L));
094300121031
094400121031         ENDDO;
094500121105
094600121105         // -?Memorizzazione posizionamento effettuato?
094700121105         if  Not Ord_x_Des;
094800121105           SavC1Clna = C1Clna;
094900121105         else;
095000121105           SavC1Dlna = C1Dlna;
095100121105         endif;
095200121031
095300121031         // -?Visualizzazione del SFL (se ci sono dati)?
095400121031         SflDsp_N = (S01nrr <= *zero);
095500121031
095600121031         // -?Attivazione del SFLEND?
095700121031         SflEnd = ( $EoF );
095800121105
095900121105         // -?Impostazione indicatore x la gestione del posizionamento?
096000121105         //  ?nel subfile?
096100121105         NoRecSfl1 = (S01nrr <= *zero);
096200121031
096300121031         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
096400121031         if  S01nrr > *zero;
096500121031           C1RcdNbr = 1;
096600121031         else;
096700121031           clear  C1RcdNbr;
096800121031         endif;
096900121031
097000121031         C1CsrRrn = C1RcdNbr;
097100121031
097200121031       ENDSR;
097300121031
097400121031       //--------------------------------------------------------------
097500121031       //?Selezione del singolo record letto dalla tabella "1AX".      ?
097600121031       //--------------------------------------------------------------
097700121031       BEGSR  sr_SelezRec;
097800121031
097900121031         reset  $RecOK;
098000121105
098100121105         if  %int( %subst( TBEke1 : 1 : %len(ORGfil) ) ) <> k_ORGfil;
098200121105           clear  Og143;
098300121105           k_ORGfil = %int( %subst( TBEke1 : 1 : %len(ORGfil) ) );
098400121105           chain  %kds(k01azorg01)  AZORG;
098500121105           if  %found(AZORG01L)  and  ORGfva = *blank;
098600121105             Og143 = ORGde3;
098700121105           endif;
098800121105         endif;
098900121031
099000121031         select;
099100121105
099200121105           // -?Esclusione per network?
099300121105           when  (Og143.§OGntw = 'DPD'  and  V1Cdpd = 'S');
099400121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
099500121105             leavesr;
099600121105           when  (Og143.§OGntw = 'EEX'  and  V1Ceex = 'S');
099700121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
099800121105             leavesr;
099900121105           when  (Og143.§OGntw = 'FED'  and  V1Cfed = 'S');
100000121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
100100121105             leavesr;
100200121031
100300121031           // -?Selezione per linea di arrivo?
100400121031           when  V1Clna_ds <> *zero  and
100500121031                 %lookup( %int( %subst( TBEke1 : 1 : %len(V1Clna1) ) )
100600121031                          : sch_D01lna ) = *zero;
100700121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
100800121031             leavesr;
100900121031
101000121031           // -?Selezione per tipo incasso contrassegno?
101100121106           //  ?(*blank <=> "--")?
101200121106           when  V1Ctic_ds <> *blank    AND
101300121106                 ( ( TBEke2  <> *blank  and
101400121106                     %lookup( %subst( TBEke2 : 1 : %len(V1Ctic1) ) :
101500121106                                            sch_D01tic ) = *zero )  OR
101600121106                   ( TBEke2  =  *blank  and
101700121106                     %lookup( c_TIC_blank : sch_D01tic ) = *zero ) );
101800121106             leavesr;
101900121031
102000121031           // -?Omissione records annullati?
102100121031           when  TBEatb <> *blank;
102200121031             yy += 1;
102300121031             if  TBEke2 = *blank;
102400121105               sch_TIC_ann(yy) = c_TIC_blank;
102500121031             else;
102600121102               sch_TIC_ann(yy) = %subst( TBEke2 : 1 : %len(S1Ctic1) );
102700121031             endif;
102800121031             leavesr;
102900121031
103000121031           // -?Richiesto posizionamento per cod. LNA export?
103100121031           //  ?si scartano i record con cod. LNA export inferiore?
103200121031           when  Not Ord_x_Des   and   TBEke1 < %editc( C1Clna : 'X' );
103300121031             leavesr;
103400121031
103500121031           // -?Richiesto posizionamento per descr. LNA export?
103600121031           //  ?si scartano i record con descr. LNA export inferiore?
103700121102           when  Ord_x_Des   and
103800121102                 %subst( TBEuni : 1 : %len(C1Dlna) ) < C1Dlna;
103900121031             leavesr;
104000121031
104100121031         endsl;
104200121031
104300121031         $RecOK = *on;
104400121031
104500121031       ENDSR;
104600121102
104700121102       //--------------------------------------------------------------
104800121102       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
104900121102       //?F8-Cambia ordinamento sfl                                    ?
105000121102       //--------------------------------------------------------------
105100121102       BEGSR  sr_F08S01;
105200121102
105300121102         Ord_x_Des = (Not Ord_x_Des);
105400121102
105500121102         select;
105600121102           // -?Subfile vuoto: nessun record da ordinare?
105700121102           when  S01nrr = *zero;
105800121102           // -?Subfile già posizionato: occorre ricaricarlo?
105900121102           when  C1Clna <> *zero  or  C1Dlna <> *blank;
106000121102             $InzS01 = *on;
106100121102           // -?Altrimenti: basta riordinarlo?
106200121102           other;
106300121102             exsr  sr_SortSfl;
106400121102         endsl;
106500121102
106600121102         if  Not Ord_x_Des;
106700121102           clear  C1Clna;
106800121102           clear  SavC1Clna;
106900121102         else;
107000121102           clear  C1Dlna;
107100121102           clear  SavC1Dlna;
107200121102         endif;
107300121102
107400121102       ENDSR;
107500121102
107600121102       //--------------------------------------------------------------
107700121102       //?Ordinamento subfile                                          ?
107800121102       //?  This subroutine sorts the subfile records.                 ?
107900121102       //--------------------------------------------------------------
108000121102       BEGSR  sr_SortSfl;
108100121102
108200121102         RrnLast  = S01nrr;
108300121102
108400121102         C1RcdNbr = 1;
108500121102         clear  C1CsrRrn;
108600121102         clear  S01nrr;
108700121102         clear  V1Dmsg;
108800121102         ErrMessage  = *off;
108900121102         SflNxtChg   = *off;
109000121102         ErrGenerico = *off;
109100121102         %subst(IndDspF : 50) = *off;
109200121102
109300121102         //?___________________________________________________________?
109400121102         //?Initialize the key fields to sort on.?
109500121102         //?There is one extra field not in the subfile -- Selected --?
109600121102         //?that is added to the record. This field is used to place?
109700121102         //?selected records in front of nonselected records.?
109800121102         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
109900121102
110000121102         clear  QLGKL;
110100121102         clear  QLGSKL;
110200121102         clear  QLGSCB;
110300121102         clear  QLGSCB00;
110400121102
110500121102         // -?Gestione della scelta ordinamento:?
110600121102         //  ?Ordinamento per cod. filiale di arrivo export?
110700121102         If  Ord_x_Des = *off;
110800121102           // -?2 campi chiave?
110900121102           QLGNBRK  = 2;
111000121102           // -?1° campo: cod. filiale di arrivo export (S1Clna)?
111100121102           //            ?a posizone    1,  3 byte, int,  ascending.?
111200121102           QLGSP    = 1;
111300121102           QLGSS    = %size(S1Clna);
111400121102           QLGDT    = Numero;
111500121102           QLGSO    = Ascendente;
111600121102           QLGKL(1) = QLGSKL;
111700121102           // -?2° campo: descr. filiale di arrivo export (S1Dlna)?
111800121102           //            ?a posizone    4, 20 byte, char, ascending.?
111900121102           QLGSP    = 4;
112000121102           QLGSS    = %size(S1Dlna);
112100121102           QLGDT    = Carattere;
112200121102           QLGSO    = Ascendente;
112300121102           QLGKL(2) = QLGSKL;
112400121102
112500121102         //  ?Ordinamento per descr. filiale di arrivo export?
112600121102         Else;
112700121102           // -?2 campi chiave?
112800121102           QLGNBRK  = 2;
112900121102           // -?1° campo: descr. filiale di arrivo export (S1Dlna)?
113000121102           //            ?a posizone    4, 20 byte, char, ascending.?
113100121102           QLGSP    = 4;
113200121102           QLGSS    = %size(S1Dlna);
113300121102           QLGDT    = Carattere;
113400121102           QLGSO    = Ascendente;
113500121102           QLGKL(1) = QLGSKL;
113600121102           // -?2° campo: cod. filiale di arrivo export (S1Clna)?
113700121102           //            ?a posizone    1,  3 byte, int,  ascending.?
113800121102           QLGSP    = 1;
113900121102           QLGSS    = %size(S1Clna);
114000121102           QLGDT    = Numero;
114100121102           QLGSO    = Ascendente;
114200121102           QLGKL(2) = QLGSKL;
114300121102
114400121102         EndIf;
114500121102
114600121102         // -?Load other sort parameters.?
114700121102         QLGLB   = 80 + 16 * MaxKey;
114800121102         QLGRL   = %size(SflRcd) - 1;
114900121102         QLGRT   = 8;
115000121102         QLGOKL  = 80;
115100121102         QLGLKE  = 16;
115200121102         QLGLSS  = 290;
115300121102         // -?Initialize Sort I/O API fields.?
115400121102         QLGRL00 = QLGRL;
115500121102         QLGRC00 = 1;
115600121102         clear  QUSEI;
115700121102         QUSBPRV = %size(QUSEC);
115800121102
115900121102         // -?First step - Initialize the sort routine.?
116000121102         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
116100121102                      SizeList : ReturnSize : QUSEC );
116200121102
116300121102         // -?Next step - Write records to I/O routine.?
116400121102         QLGRT00  = Put;
116500121102         For  Nrr = 1  To  RrnLast;
116600121102           chain  Nrr  tb1AXs01;
116700121102           // -?Solo le righe con Selected = 'Y' sono riordinate,?
116800121102           //  ?quindi per fare un ordinamento di tutte le righe?
116900121102           //  ?metto 'Y' sempre.?
117000121102           Selected = 'Y';
117100121102           clear  QUSEI;
117200121102           QUSBPRV  = %size(QUSEC);
117300121102           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
117400121102                         SizeList : NotUsed : QUSEC );
117500121102         EndFor;
117600121102
117700121102         // -?Next step - Signal end of input, clear subfile for reload.?
117800121102         QLGRT00 = EndPut;
117900121102         clear  QUSEI;
118000121102         QUSBPRV = %size(QUSEC);
118100121102         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
118200121102                       SizeList : NotUsed : QUSEC );
118300121102
118400121102         // -?Pulizia subfile?
118500121102         SflDsp_N    = *on;
118600121102         SflDspCtl_N = *on;
118700121102         write  tb1AXc01;
118800121102         SflDspCtl_N = *off;
118900121102         SflEnd      = *off;
119000121102
119100121102         // -?Final step - Write the records back to the subfile.?
119200121102         QLGRT00  = Get;
119300121105         For  Nrr = 1  To  RrnLast;
119400121102           clear  QUSEI;
119500121102           QUSBPRV = %size(QUSEC);
119600121102           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
119700121102                         QLGRL00  : NotUsed : QUSEC );
119800121102           // -?Ripristino indicatori utilizzati nel sfl rec?
119900121102           SflNxtChg = (S1Copz <> *zero);
120000121102           S01nrr = Nrr;
120100121102           write  tb1AXs01;
120200121102         EndFor;
120300121102
120400121102         C1CsrRrn = 1;
120500121102
120600121102         // -?Visualizzazione del SFL (se ci sono dati)?
120700121102         SflDsp_N = (S01nrr <= *zero);
120800121102
120900121102         // -?Attivazione del SFLEND?
121000121102         SflEnd = $EoF;
121100121102
121200121102       ENDSR;
121300121031
121400121031       //--------------------------------------------------------------
121500121031       //?Caricamento completo del subfile S01                         ?
121600121031       //--------------------------------------------------------------
121700121031       BEGSR  sr_OpzS01;
121800121031
121900121031         clear  SavS01csr;
122000121105
122100121105         // -?Spegnimento degli indicatori di errore?
122200121105         %subst(IndDspF : 50) = *off;
122300121031
122400121031         // -?Ciclo di lettura subfile?
122500121031         readc  tb1AXs01;
122600121031
122700121031         DoW  Not %eof(TNTB1AXD);
122800121031
122900121031           SflNxtChg = *off;
123000121031           %subst(IndDspF : 50) = *off;
123100121031           C1RcdNbr = S01nrr;
123200121031
123300121031           select;
123400121031
123500121031             // -?Opz. 2=Modifica?
123600121031             //  ?Opz. 3=Copia?
123700121031             //  ?Opz. 4=Annullamento/Ripristino?
123800121031             //  ?Opz. 5=Visualizzazione?
123900121031             when  S1Copz = 2  or  S1Copz = 3  or  S1Copz = 4  or
124000121031                   S1Copz = 5;
124100121031               $Video  = 'S2';
124200121031               $InzS02 = *on;
124300121031               DoW  $Video = 'S2'  and  Not $Fine;
124400121031                 exsr  sr_GesS02;
124500121031               EndDo;
124600121031
124700121031             // -?Nessuna opzione?
124800121031             when  S1Copz = *zero;
124900121031
125000121031             // -?Opzione errata?
125100121031             other;
125200121031               ErrGenerico = *on;
125300121031               ErrMessage  = *on;
125400121031               PosCurOPZ   = *on;
125500121031               V1Dmsg = $Msg(01);
125600121031               $Fine  = *off;
125700121031
125800121031           endsl;
125900121031
126000121031           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
126100121031           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
126200121031           if  S1Copz  <> *zero    and
126300121031              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
126400121031             SavS01csr   = C1RcdNbr;
126500121031           endif;
126600121031
126700121031           // -?Aggiornamento sfl?
126800121031           select;
126900121031             when  ErrMessage;
127000121031               SflNxtChg = *on;
127100121031               C1CsrRrn  = C1RcdNbr;
127200121031             when  ErrGenerico;
127300121031               C1CsrRrn  = C1RcdNbr;
127400121031               clear  S1Copz;
127500121031             other;
127600121031               clear  S1Copz;
127700121031           endsl;
127800121031
127900121031           UPDATE  tb1AXs01;
128000121031
128100121031           if  ErrGenerico   or   ErrMessage;
128200121031             leave;
128300121031           endif;
128400121031
128500121031           readc  tb1AXs01;
128600121031
128700121031         ENDDO;
128800121031
128900121031         // -?Riposizionam. cursore sul record contenente la prima opz.?
129000121031         //  ?(se non sono stati rilevati errori)?
129100121031         if  NOT ErrMessage   and   NOT ErrGenerico   and
129200121031             SavS01csr > *zero;
129300121031           C1CsrRrn = SavS01csr;
129400121031         endif;
129500121031
129600121031       ENDSR;
129700121031
129800121031       //--------------------------------------------------------------
129900121031       //?Gestione subfile S02.                                        ?
130000121031       //--------------------------------------------------------------
130100121031       BEGSR  sr_GesS02;
130200121031
130300121031         // -?Inizializzazione videata?
130400121031         if  $InzS02 = *on;
130500121031           exsr  sr_InzS02;
130600121031           $InzS02 = *off;
130700121031         endif;
130800121031
130900121031         // -?Emissione Testata & Piede?
131000121031         write  tb1AXt01;
131100121031         write  tb1AXp02;
131200121031
131300121031         // -?Posizionamento cursore?
131400121031         //  ?C2CsrRrn contiene il numero di riga del subfile su cui?
131500121031         //  ?era posizionato il cursore; impostandolo in C2RcdNbr?
131600121031         //  ?si visualizza la pagina che vedeva l'utente quando ha?
131700121031         //  ?premuto l'ultimo tasto.?
131800121031         if  C2CsrRrn > *zero;
131900121031           C2RcdNbr = C2CsrRrn;
132000121031         endif;
132100121031
132200121031         // -?Emissione videata?
132300121031         //  ?(protetta se visualizzazione o annullamento)?
132400121031         if  Not $Immissione  AND
132500121031            (S1Copz = 5       or
132600121105             S1Copz = 4);
132700121105            //(S1Copz = 4  and  TBEatb = *blank));
132800121031           write  tb1AXc02;
132900121031           exfmt  Protect;
133000121031         else;
133100121031           exfmt  tb1AXc02;
133200121031         endif;
133300121031
133400121031         reset  ErrGenerico;
133500121031         reset  ErrMessage;
133600121031         clear  V1Dmsg;
133700121031
133800121031         Select;
133900121031
134000121031           // -?F3=Fine?
134100121031           When  dsp_aid = c_F03;
134200121031             unlock  TNTBE01L;
134300121031             //$Fine  = *on;
134400121031             $Video = 'S1';
134500121031             ErrGenerico = *on;
134600121031
134700121031           // -?F12=Ritorno?
134800121031           When  dsp_aid = c_F12;
134900121031             unlock  TNTBE01L;
135000121031             $Video = 'S1';
135100121031
135200121105           // -?Invio, F6=Conferma?
135300121031           Other;
135400121031             // -?Controlli eseguiti NON se Annullamento?
135500121105             if  $Immissione   or  S1Copz <> 4;
135600121105                 //or  (S1Copz = 4  and  TBEatb <> *blank);
135700121031               exsr  sr_CtrC02;
135800121031             endif;
135900121031             if  ErrGenerico;
136000121031               leavesr;
136100121031             endif;
136200121031             // -?Aggiornamento?
136300121031             if  dsp_aid = c_F06;
136400121031               $Video  = 'W1';
136500121031               $InzW01 = *on;
136600121031               DoW  $Video = 'W1';
136700121031                 exsr  sr_GesW01;
136800121031               EndDo;
136900121031             endif;
137000121031
137100121031         EndSl;
137200121031
137300121031       ENDSR;
137400121031
137500121031       //--------------------------------------------------------------
137600121031       //?Inizializzazione subfile S02.                                ?
137700121031       //--------------------------------------------------------------
137800121031       BEGSR  sr_InzS02;
137900121031
138000121031         // -?Spegnimento degli indicatori di errore?
138100121031         %subst(IndDspF : 50) = *off;
138200121031
138300121031         // -?Reperimento dati dal file?
138400121031         clear  xx;
138500121031         clear  sch_TIC_ds;
138600121105         clear  sch_TICd_ds;
138700121105         IF  dsp_aid <> c_F06;
138800121031           clear  k05tntbe01;
138900121031           k_TBEcod = c_Tab;
139000121031           k_TBEke1 = S1Clna;
139100121031           setll  %kds( k05tntbe01 : 2 )  TNTBE000;
139200121105           if  S1Copz = 3  or  S1Copz = 5;
139300121105             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
139400121105           else;
139500121105             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
139600121105           endif;
139700121031           DoW  Not %eof(TNTBE01L);
139800121031             if  TBEatb = *blank;
139900121031               xx += 1;
140000121031               if  TBEke2 = *blank;
140100121105                 sch_TIC(xx) = c_TIC_blank;
140200121031               else;
140300121102                 sch_TIC(xx) = %subst( TBEke2 : 1 : %len(S1Ctic1) );
140400121031               endif;
140500121031             endif;
140600121105             if  S1Copz = 3  or  S1Copz = 5;
140700121105               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
140800121105             else;
140900121105               reade  %kds( k05tntbe01 : 2 )  TNTBE000;
141000121105             endif;
141100121031           EndDo;
141200121031         ENDIF;
141300121031
141400121031         // -?Pulizia subfile-control?
141500121031         clear  tb1AXc02;
141600121031
141700121031         // -?Pulizia subfile?
141800121031         Sfl2Dsp_N    = *on;
141900121031         Sfl2DspCtl_N = *on;
142000121031         write  tb1AXc02;
142100121031         Sfl2DspCtl_N = *off;
142200121031         Sfl2End      = *off;
142300121031
142400121031         clear  C2RcdNbr;
142500121031         clear  C2CsrRrn;
142600121031         clear  S02nrr;
142700121031         clear  V1Dmsg;
142800121031         ErrMessage  = *off;
142900121031         ErrGenerico = *off;
143000121031
143100121031         // -?Caricamento dei dati nel subfile-control?
143200121031         select;
143300121105            when  dsp_aid = c_F06;
143400121031              V2Topz = '  INSERIMENTO  ';
143500121031            when  S1Copz  = 2;
143600121031              V2Topz = '   MODIFICA    ';
143700121031            when  S1Copz  = 3;
143800121031              V2Topz = '     COPIA     ';
143900121031            when  S1Copz  = 4;
144000121031              V2Topz = ' ANNULLAMENTO  ';
144100121031            //when  S1Copz  = 4  and  TBEatb <> *blank;
144200121031            //  V2Topz = '  RIPRISTINO   ';
144300121031            when  S1Copz  = 5;
144400121031              V2Topz = 'VISUALIZZAZIONE';
144500121031         endsl;
144600121031
144700121031         // ·?Protezione campi chiave SE NON inserimento?
144800121105         $Immissione = (dsp_aid = c_F06);
144900121105         ProtectKEY  = (dsp_aid <> c_F06  and  S1Copz <> 3);
145000121031
145100121031         // -?Impostazione LNA export  e?
145200121031         //  ?Caricamento dei tipi incasso c/assrgno?
145300121031         //  ?(SE NON immissione o copia)?
145400121105         If  dsp_aid <> c_F06  and  S1Copz <> 3;
145500121031
145600121031           // -?Filiale di arrivo per export?
145700121031           C02lna = %int( %trim(TBEke1) );
145800121031           k_ORGfil = C02lna;
145900121031           chain  %kds(k01azorg01)  AZORG;
146000121031           if  %found(AZORG01L);
146100121031             C02lnaD = ORGdes;
146200121031           endif;
146300121031
146400121031         EndIf;
146500121105
146600121108         // -?Note: indicazione di "  " visualizzata come "--"?
146700121105         clear  ds_TNTBE.TBEke1;
146800121105         if  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
146900121105                          *omit : *omit : *omit :
147000121105                          *omit : *omit :
147100121105                          *omit : *omit : *omit : *omit :
147200121105                          *omit : *omit : *omit : *omit :
147300121105                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
147400121105                        = *zero   AND
147500121105                        ds_TNTBE.TBEatb = *blank;
147600121105           ds1A   = ds_TNTBE.TBEuni;
147700121113           C02note = '("--" = blank - ' + %trim(§1Ades) +')';
147800121105         endif;
147900121031
148000121031         // -?Tipi incasso c/assegno?
148100121031         For  xx = 1  To  c_MaxTIC;
148200121031
148300121031           clear  tb1AXs02;
148400121031
148500121031           If  sch_TIC(xx) <> *blank;
148600121031
148700121031             Sfl2NxtChg = *on;
148800121031             S2Ctic = sch_TIC(xx);
148900121105             if  S2Ctic = c_TIC_blank;
149000121105               clear  ds_TNTBE.TBEke1;
149100121105             else;
149200121105               ds_TNTBE.TBEke1 = S2Ctic;
149300121105             endif;
149400121031             clear  ds_TNTBE.TBEke2;
149500121031             if  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
149600121031                              *omit : *omit : *omit :
149700121031                              *omit : *omit :
149800121031                              *omit : *omit : *omit : *omit :
149900121031                              *omit : *omit : *omit : *omit :
150000121031                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
150100121031                            = *zero   AND
150200121031                            ds_TNTBE.TBEatb = *blank;
150300121031               ds1A   = ds_TNTBE.TBEuni;
150400121031               S2Dtic = §1Ades;
150500121031             else;
150600121031               S2Dtic = *all'? ';
150700121031             endif;
150800121031
150900121031           EndIf;
151000121031
151100121031           // -?Scrittura record nel subfile?
151200121031           S02nrr += 1;
151300121031           write  tb1AXs02;
151400121031
151500121031         EndFor;
151600121031
151700121031         // -?Posizionamento cursore sulla linea di arrivo export?
151800121031         //  ?(SE immissione)?
151900121105         PosCurLNAc2 = (dsp_aid = c_F06  or  S1Copz = 3);
152000121031
152100121031         // -?Visualizzazione del SFL  (SE ci sono dati)?
152200121031         Sfl2Dsp_N = (S02nrr <= *zero);
152300121031
152400121031         // -?Attivazione del SFLEND?
152500121031         Sfl2End = *on;
152600121031
152700121031         // -?Impaginazione subfile?
152800121031         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
152900121031         if S02nrr > *zero;
153000121031           C2RcdNbr  = 1;
153100121031           C2CsrRrn  = 1;
153200121031         else;
153300121031           clear C2RcdNbr;
153400121031         endif;
153500121031
153600121031         // -?Abilitazione tasti funzionali condizionati?
153700121102         F3Attivo  = *on;
153800121031         F6Attivo  = ($Immissione  or  S1Copz <> 5);
153900121102         F8Attivo  = (S01nrr > 1);
154000121102         F9Attivo  = (S01nrr > *zero);
154100121102         F12Attivo = *on;
154200121031
154300121031       ENDSR;
154400121031
154500121031       //--------------------------------------------------------------
154600121031       //?Controllo dati nella videata C02/S02.                        ?
154700121031       //--------------------------------------------------------------
154800121031       BEGSR  sr_CtrC02;
154900121031
155000121031         // -?Spegnimento degli indicatori di errore?
155100121031         %subst(IndDspF : 50) = *off;
155200121031
155300121031         // -?Controllo linea di arrivo export?
155400121031         // ·?Obbligatoria?
155500121031         if  C02lna = *zero;
155600121031           ErrGenerico = *on;
155700121031           ErrMessage  = *on;
155800121105           PosCurLNAc2 = *on;
155900121031           V1Dmsg = $Msg(03);
156000121031           leavesr;
156100121031         endif;
156200121031         // ·?Errata?
156300121102         k_ORGfil = C02lna;
156400121031         chain  %kds(k01azorg01)  AZORG;
156500121031         if  Not %found(AZORG01L)   or   ORGfva <> *blank   or
156600121031             (ORGfag <> 'F'   and   ORGfag <> 'A');
156700121031           ErrGenerico = *on;
156800121031           ErrMessage  = *on;
156900121105           PosCurLNAc2 = *on;
157000121031           V1Dmsg = $Msg(03);
157100121031           leavesr;
157200121031         endif;
157300121031         C02lnaD = ORGdes;
157400121106         Og143 = ORGde3;
157500121106         if  Og143.§OGntw <> 'DPD'  and
157600121106             Og143.§OGntw <> 'EEX'  and
157700121106             Og143.§OGntw <> 'FED';
157800121106           ErrGenerico = *on;
157900121106           ErrMessage  = *on;
158000121106           PosCurLNAc2 = *on;
158100121106           V1Dmsg = $Msg(03);
158200121106           leavesr;
158300121106         endif;
158400121031
158500121031         // -?Controllo SE chiave duplicata (SE immissione)?
158600121105         If  $Immissione  or  S1Copz = 3;
158700121031           clear  k05tntbe01;
158800121031           k_TBEcod = c_Tab;
158900121031           k_TBEke1 = %editc( C02lna : 'X' );
159000121031           setll  %kds( k05tntbe01 : 2 )  TNTBE000;
159100121105           reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
159200121105           dow  Not %eof(TNTBE01L);
159300121105             if  TBEatb = *blank;
159400121105               ErrGenerico = *on;
159500121105               ErrMessage  = *on;
159600121105               PosCurLNAc2 = *on;
159700121105               V1Dmsg = $Msg(04);
159800121105               leavesr;
159900121105             endif;
160000121105             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
160100121105           enddo;
160200121031         EndIf;
160300121031
160400121031         // -?Controllo tipi incasso c/assegno nel subfile S02?
160500121031         clear  sch_TIC;
160600121105         clear  sch_TICd;
160700121031         readc  tb1AXs02;
160800121031
160900121031         DoW  Not %eof(TNTB1AXD);
161000121031
161100121031           exsr  sr_CtrS02;
161200121031
161300121102           // ·?Aggiornamento sfl?
161400121031           select;
161500121031             when  ErrMessage;
161600121031               Sfl2NxtChg = *on;
161700121031               C2CsrRrn  = C2RcdNbr;
161800121031             when  ErrGenerico;
161900121105               Sfl2NxtChg = *on;
162000121031               C2CsrRrn  = C2RcdNbr;
162100121106             when  S2Ctic <> *blank;
162200121106               Sfl2NxtChg = *on;
162300121106               //C2CsrRrn  = C2RcdNbr;
162400121031           endsl;
162500121031           UPDATE  tb1AXs02;
162600121031
162700121031           if  ErrGenerico   or   ErrMessage;
162800121108             leavesr;
162900121031           endif;
163000121031
163100121031           readc  tb1AXs02;
163200121031
163300121031         EndDo;
163400121108
163500121108         C2RcdNbr  = 1;
163600121108         C2CsrRrn  = C2RcdNbr;
163700121105
163800121105         // -?Almeno un Tipo incasso c/assegno obbligatorio?
163900121105         if  sch_TIC_ds = *blank;
164000121105           ErrGenerico = *on;
164100121105           ErrMessage  = *on;
164200121105           PosCurTICs2 = *on;
164300121105           V1Dmsg = $Msg(07);
164400121105           leavesr;
164500121105         endif;
164600121031
164700121031       ENDSR;
164800121031
164900121031       //--------------------------------------------------------------
165000121031       //?Controllo dati nel subfile S02.                              ?
165100121031       //--------------------------------------------------------------
165200121031       BEGSR  sr_CtrS02;
165300121105
165400121105         C2RcdNbr = S02nrr;
165500121105
165600121105         // -?Spegnimento degli indicatori di errore?
165700121105         %subst(IndDspF : 50) = *off;
165800121105
165900121031         clear  S2Dtic;
166000121105
166100121105         // -?Tipo incasso c/assegno: ricerca?
166200121105         If  %scan('?' : S2Ctic) > *zero;
166300121105
166400121105           clear  S2Ctic;
166500121105           TABEL_Ricerca( '1A' :
166600121105                          *blank :
166700121105                          idElemento :
166800121105                          tastoFunzionaleUscita );
166900121105           if  tastoFunzionaleUscita = *blank;
167000121105             if  idElemento = *blank;
167100121105               S2Ctic = c_TIC_blank;
167200121105             else;
167300121105               S2Ctic = idElemento;
167400121105             endif;
167500121105           else;
167600121105             ErrGenerico = *on;
167700121105           endif;
167800121105           PosCurTICs2 = *on;
167900121105
168000121105         EndIf;
168100121105
168200121105         // -?Tipo incasso c/assegno: controllo?
168300121105         //If  S2Ctic <> *blank  and  S2Ctic <> *all'9';
168400121105         If  S2Ctic <> *blank;
168500121105
168600121105           // ·?Ripetuto nel subfile?
168700121105           if  %lookup( S2Ctic : sch_TIC ) > *zero;
168800121105             ErrGenerico = *on;
168900121105             ErrMessage  = *on;
169000121105             PosCurTICs2 = *on;
169100121105             V1Dmsg = $Msg(05);
169200121105             leavesr;
169300121105           endif;
169400121031
169500121105           // ·?Controllo/Decodifica?
169600121105           if  S2Ctic = c_TIC_blank;
169700121105             clear  ds_TNTBE.TBEke1;
169800121105           else;
169900121105             ds_TNTBE.TBEke1 = S2Ctic;
170000121105           endif;
170100121105           clear  ds_TNTBE.TBEke2;
170200121105           If  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
170300121105                            *omit : *omit : *omit :
170400121105                            *omit : *omit :
170500121105                            *omit : *omit : *omit : *omit :
170600121105                            *omit : *omit : *omit : *omit :
170700121105                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
170800121105                          = *zero   AND
170900121105                          ds_TNTBE.TBEatb = *blank;
171000121105             ds1A   = ds_TNTBE.TBEuni;
171100121105             S2Dtic = §1Ades;
171200121105           Else;
171300121105             S2Dtic = *all'? ';
171400121105             ErrGenerico = *on;
171500121105             ErrMessage  = *on;
171600121105             PosCurTICs2 = *on;
171700121105             V1Dmsg = $Msg(06);
171800121105             leavesr;
171900121105           EndIf;
172000121105
172100121105           // ·?Memorizzazione Tipo incasso c/assegno in schiera?
172200121105           //  ?(SE corretto)?
172300121105           xx = %lookup( *blank : sch_TIC);
172400121105           if  xx > *zero;
172500121105             sch_TIC(xx)  = S2Ctic;
172600121105             sch_TICd(xx) = S2Dtic;
172700121105           endif;
172800121031
172900121105         EndIf;
173000121031
173100121031       ENDSR;
173200121031
173300121031       //--------------------------------------------------------------
173400121031       //?Gestione videata W01 (dati relativi alla trasmissione).      ?
173500121031       //--------------------------------------------------------------
173600121031       BEGSR  sr_GesW01;
173700121031
173800121031         // -?Inizializzazione videata?
173900121031         if  $InzW01 = *on;
174000121031           exsr  sr_InzW01;
174100121031           $InzW01 = *off;
174200121031         endif;
174300121031
174400121031         // -?Emissione window?
174500121031         IF  TBXftt = 'S';
174600121031
174700121031           exfmt  tb1AXw01;
174800121031
174900121031           reset  ErrGenerico;
175000121031           reset  ErrMessage;
175100121031           clear  V1Dmsg;
175200121031
175300121031           Select;
175400121031
175500121031             // -?F12=Ritorno?
175600121031             When  dsp_aid = c_F12;
175700121031               $Video = 'S2';
175800121031               leavesr;
175900121031
176000121031             // -?Invio?
176100121031             When  dsp_aid <> c_F06;
176200121031               leavesr;
176300121031
176400121031           EndSl;
176500121031
176600121031         ENDIF;
176700121031
176800121031         // -?F6=Aggiornamento TNTBE00F?
176900121031         //  ?prima annulla TUTTE le tab."1AX" con la LNA export in KE1,?
177000121031         //  ?poi   vi registra TUTTI i tipi incasso del subfile.?
177100121105         clear  k05tntbe01;
177200121105         k_TBEcod = c_Tab;
177300121105         k_TBEke1 = %editc( C02lna : 'X' );
177400121102         exsr  sr_AnnullTNTBE;
177500121105         IF  $Immissione  or  S1Copz <> 4;
177600121105           For  xx = 1  To  %elem(sch_TIC);
177700121105             if  sch_TIC(xx) <> *blank;
177800121105               exsr  sr_UpdTNTBE;
177900121105               if  ErrGenerico;
178000121105                 leave;
178100121105               endif;
178200121105             endif;
178300121105           EndFor;
178400121105         ENDIF;
178500121031
178600121031         If  ErrGenerico = *on;
178700121031           // -?Rilevato errore in fase di aggiornamento?
178800121031           leavesr;
178900121031         Else;
179000121102           // -?SubFile S01 altrimenti?
179100121031           $Video  = 'S1';
179200121105           $InzS01 = ($Immissione  or  S1Copz = 3  or  S1Copz = 4);
179300121102           if  not  $InzS01;
179400121102             S1Ctic_ds = sch_TIC_ds;
179500121031           endif;
179600121031         EndIf;
179700121031
179800121031       ENDSR;
179900121031
180000121031       //--------------------------------------------------------------
180100121031       //?Inizializzazione videata W01                                 ?
180200121031       //--------------------------------------------------------------
180300121031       BEGSR  sr_InzW01;
180400121031
180500121031         clear tb1AXw01;
180600121031
180700121031         select;
180800121031           // -?Opz.3=Copia?
180900121031           when  S1Copz = 3;
181000121031             W1ftt = TBXftt;
181100121031           // -?Opz.4=Ripristino?
181200121031           //when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = 'A';
181300121031           //  W1ftt = TBEftt;
181400121031           // -?Opz.4=Annullamento?
181500121031           when  S1Copz = 4;
181600121031             W1ftt = TBEftt;
181700121031           // -?Opz.2=Modifica?
181800121031           when  S1Copz = 2;
181900121031             W1ftt = TBEftt;
182000121105           // -?F6=Immissione?
182100121031           when  $Immissione;
182200121031             W1ftt = TBXftt;
182300121031         endsl;
182400121031
182500121031         // -?Se NON immissione:?
182600121031         //  ?visualizza i dati relativi all'ultima trasmissione?
182700121031         if  %found(TNTBE01L);
182800121031           W1flt  = TBEflt;
182900121031           W1ftr  = TBEftr;
183000121031           if  TBEdtr <> *zero;
183100121031             wData_Eur = %date( TBEdtr : *iso );
183200121031             W1dtr     = %dec( wData_Eur );
183300121031           endif;
183400121031         endif;
183500121031
183600121031       ENDSR;
183700121031
183800121031       //--------------------------------------------------------------
183900121031       //?Annullamento record gia inseriti in TNTBE00F (tab. "1AX").   ?
184000121031       //--------------------------------------------------------------
184100121031       BEGSR  sr_AnnullTNTBE;
184200121031
184300121105         //clear  k05tntbe01;                   ?(già fatto)?
184400121105         //k_TBEcod = c_Tab;                    ?(già così)?
184500121105         //k_TBEke1 = %editc( C02lna : 'X' );   ?(già così)?
184600121031         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
184700121031         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
184800121031
184900121031         DoW  not %eof(TNTBE01L);
185000121105
185100121105           If  TBEatb <> 'A';
185200121031
185300121105             TBEftt = W1ftt;
185400121105             TBEflt = W1flt;
185500121105             clear TBEftr;
185600121105             TBEatb = 'A';
185700121105             //_______________
185800121105             UPDATE  TNTBE000;
185900121105             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
186000121105
186100121105           EndIf;
186200121102
186300121031           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
186400121031
186500121031         EndDo;
186600121031
186700121031       ENDSR;
186800121031
186900121031       //--------------------------------------------------------------
187000121031       //?Aggiornamento singolo record in TNTBE00F (tab. "1AX").       ?
187100121031       //--------------------------------------------------------------
187200121031       BEGSR  sr_UpdTNTBE;
187300121031
187400121102         // -?Aggancio record da aggiornare/inserire/annullare?
187500121031         //clear  k05tntbe01;                   ?(già fatto)?
187600121031         //k_TBEcod = c_Tab;                    ?(già così)?
187700121031         //k_TBEke1 = %editc( C02lna : 'X' );   ?(già così)?
187800121105         if  sch_TIC(xx) = c_TIC_blank;
187900121031           clear  k_TBEke2;
188000121031         else;
188100121105           k_TBEke2 = sch_TIC(xx);
188200121031         endif;
188300121031         chain  %kds(k05tntbe01)  TNTBE000;
188400121031
188500121031         // -?Impostazione dei campi chiave  SE immissione?
188600121031         if  NOT %found(TNTBE01L);
188700121031           clear  TNTBE000;
188800121031           TBEcod = k_TBEcod;
188900121031           TBEke1 = k_TBEke1;
189000121031           TBEke2 = k_TBEke2;
189100121031         endif;
189200121031
189300121031         // -?Impostazione dati (se NON annullamento)?
189400121106         //  ?(NON potrà MAI essere un annullamento:?
189500121106         //  ? tutti i rec. della LNA sono GIÀ stati annullati)?
189600121102         //if  S1Copz <> 4;
189700121031           TBEapl = TBXapl;
189800121105           TBEuni = C02lnaD + sch_TICd(xx);
189900121102         //endif;
190000121031
190100121031         TBEftt = W1ftt;
190200121031         TBEflt = W1flt;
190300121031         clear TBEftr;
190400121031         //clear TBEdtr;
190500121031
190600121102         select;
190700121102           when  $Immissione;          //?- inserimento?
190800121102             //clear  TBEatb;         ?(già così)?
190900121102           when  S1Copz <> 4;          //?- modifica/copia?
191000121102             clear  TBEatb;
191100121102           other;                      //?- annullamento?
191200121102             //TBEatb = 'A';          ?(NON potrà MAI essere annullam.)?
191300121102         endsl;
191400121031
191500121031         IF  NOT  %found(TNTBE01L);
191600121031           //_________________
191700121031           WRITE(e)  TNTBE000;
191800121031           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191900121031           if  %error;
192000121031             ErrGenerico = *on;
192100121031             ErrMessage  = *on;
192200121105             PosCurTICs2 = *on;
192300121105             V1Dmsg = %trimr($Msg(08)) + S2Ctic + '"';
192400121031             leavesr;
192500121031           endif;
192600121031         ELSE;
192700121031           //_______________
192800121031           UPDATE  TNTBE000;
192900121031           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193000121031         ENDIF;
193100121031
193200121031       ENDSR;
193300121031
193400121031       //--------------------------------------------------------------
193500121031       //?Operazioni finali.                                           ?
193600121031       //--------------------------------------------------------------
193700121031       BEGSR  sr_RoutEnd;
193800121106
193900121106         // -?Chiusura funzioni precedentemente aperte?
194000121106         cloTabella ( c_Tabel );
194100121031
194200121106         // -?Chiusura *pgm?
194300121031         return;
194400121031
194500121031       ENDSR;
194600121031
194700121031      /end-free
194800121031
194900121031** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
195000121031Opzione errata                                                                  1
195100121031Dati annullati: usare l'opzione 4 per ripristinarli e modificarli               2
195200121106Linea di arrivo EXPORT errata o mancante                                        3
195300121031Linea di arrivo export già presente in tabella                                  4
195400121105Tipo incasso c/assegno già indicato                                             5
195500121105Tipo incasso c/assegno errato                                                   6
195600121105Specificare almeno un tipo incasso c/assegno                                    7
195700121105Rilevato errore in fase di registrazione tipo incasso c/assegno "               8
195800121105Lasciare inclusa almeno un tipo di linee di arrivo (DPD, EuroExpress o FedEx)   9
195900121105Linea di arrivo già indicata                                                   10
196000121106Linea di arrivo EXPORT errata                                                  11
196100121106Linea di arrivo export esclusa come                                            12
