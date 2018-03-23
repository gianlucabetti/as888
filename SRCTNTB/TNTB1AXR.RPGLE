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
034400121120     d   S1Cpiu                            inz
034500121102     d   S1Copz                            inz
034600121102     d   Selected                     1a   inz
034700121031      /copy QSYSINC/QRPGLESRC,QLGSORT
034800121031     d QLGKL                         16    dim(MaxKey)
034900121102     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
035000121102     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
035100121102     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
035200121102     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
035300121031      /copy QSYSINC/QRPGLESRC,QLGSRTIO
035400121031      /copy QSYSINC/QRPGLESRC,QUSEC
035500121031
035600121031       //--------------------------------------------------------------
035700121031       // -?Standalone fields?
035800121031       //  ?Nrr        - Relative record number for subfile?
035900121031       //  ?SizeList   - Size of list?
036000121031       //  ?ReturnSize - Size of list returned by sort API?
036100121031       //--------------------------------------------------------------
036200121031     d NotUsed         s             16a   inz
036300121031     d ReturnSize      s              9b 0 inz
036400121031     d SizeList        s              9b 0 inz
036500121031     d RrnLast         s              5i 0 inz
036600121031     d Nrr             s              5i 0 inz
036700121031
036800121031       //--------------------------------------------------------------
036900121031       //?Definizione prototipi procedure.                             ?
037000121031       //--------------------------------------------------------------
037100121031
037200121031       // -?Reperimento dati utente?
037300121031     d TIBS34ds      e ds                  inz
037400121031      /copy gaitrasrc/srcProtoPR,TIBS34R
037500121105
037600121105       // -?Ricerca tabella TABEL00F?
037700121105     d idElemento      s              8    inz
037800121105     d tastoFunzionaleUscita...
037900121105     d                 s              1    inz
038000121105      /copy gaitrasrc/srcProtoPR,TRUL19R
038100121031
038200121031       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
038300121031     d c_TABEL         c                   const('1')
038400121031       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
038500121031     d c_TNTBE         c                   const('2')
038600121031       // -?Reperimento dati tabelle?
038700121031      /copy gaitrasrc/srcProtoPR,TRULTAB
038800121031
038900121031       // -?Ordinamento subfile?
039000121031      /copy gaitrasrc/srcProtoPr,QLGSRTIO
039100121031
039200121031       //--------------------------------------------------------------
039300121031       //?Definizione key-list.                                        ?
039400121031       //--------------------------------------------------------------
039500121031
039600121031       // -?File TNTBE01L?
039700121031     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
039800121031     d                                     prefix(k_)   inz
039900121031
040000121031       // -?File AZORG01L?
040100121031     d k01azorg01    e ds                  extname(AZORG01L : *key)
040200121031     d                                     prefix(k_)   inz
040300121031
040400121031       //--------------------------------------------------------------
040500121031       //?Riepilogo indicatori utilizzati.                             ?
040600121031       //--------------------------------------------------------------
040700121031
040800121031
040900121031       //--------------------------------------------------------------
041000121031       //?M A I N - L I N E                                            ?
041100121031       //--------------------------------------------------------------
041200121031
041300121031     c     *Entry        plist
041400121031     c                   parm                    KPJBA
041500121031
041600121031      /free
041700121031
041800121031       // -?Operazioni iniziali?
041900121031       exsr  sr_RoutInz;
042000121031
042100121031       // -?Ciclo di gestione del file video?
042200121031       DoW  $Fine = *off;
042300121031
042400121031         select;
042500121031
042600121031           // -?Gestione videata S1 (subfile)?
042700121031           when  $Video = 'S1';
042800121031             exsr  sr_GesS01;
042900121031
043000121031           // -?Gestione videata S2 (subfile)?
043100121031           //  ?(da qui solo per "immissione")?
043200121031           when  $Video = 'S2';
043300121031             exsr  sr_GesS02;
043400121102
043500121102           // -?Gestione videata D1 (parzializzazioni)?
043600121102           when  $Video = 'D1';
043700121102             exsr  sr_GesD01;
043800121031
043900121031           // -?Richiesta trasmissione dati?
044000121031           //  ?(NON da qui)?
044100121031           //when  $Video = 'W1';
044200121031           //  exsr  sr_GesW01;
044300121031
044400121031           // -?? ? ??
044500121031           other;
044600121031             $Fine = *on;
044700121031
044800121031         endsl;
044900121031
045000121031       EndDo;
045100121031
045200121031       // -?Operazioni finali?
045300121031       exsr  sr_RoutEnd;
045400121031
045500121031       //--------------------------------------------------------------
045600121031       //?Operazioni iniziali.                                         ?
045700121031       //--------------------------------------------------------------
045800121031       BEGSR  sr_RoutInz;
045900121031
046000121031         // -?Impostazione chiusura?
046100121031         *inLR = *on;
046200121031
046300121031         // -?Reperimento dati job?
046400121031         exsr  sr_DatiJob;
046500121031
046600121031         // -?Impostazione nome programma a video?
046700121031         V1Tpgm = SDSpgm;
046800121031
046900121031         // -?Aggancio dati generali della tabella in esame?
047000121031         clear  k_TBEcod;
047100121031         k_TBEke1 = *zero;
047200121031         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
047300121031                = c_Tab;
047400121031         clear  k_TBEke2;
047500121031         clear  k_TBElin;
047600121031         k_TBEsif = KNSIF;
047700121031         chain  %kds(k05tntbe01)  TNTBE000;
047800121031         if  not %found(TNTBE01L);
047900121031           clear  k_TBEsif;
048000121031           chain  %kds(k05tntbe01)  TNTBE000;
048100121031         endif;
048200121031         if  %found(TNTBE01L);
048300121031           xTNTBEds = TNTBEds;
048400121031         else;
048500121031           clear  xTNTBEds;
048600121031         endif;
048700121031
048800121031         // -?Pulizia / impostazione iniziale dei campi chiave?
048900121031         clear  k05tntbe01;
049000121031         k_TBEcod = c_Tab;
049100121106
049200121106         // -?Impostazione videata iniziale?
049300121106         $Video = 'S1';
049400121106         reset  $InzS01;
049500121106         reset  $InzS02;
049600121106         reset  $InzD01;
049700121106         exsr  sr_InzD01;
049800121106         $InzD01 = *off;
049900121031
050000121031       ENDSR;
050100121031
050200121031       //--------------------------------------------------------------
050300121031       //?Reperimento Dati del job (Utente/Operativi).                 ?
050400121031       //--------------------------------------------------------------
050500121031       BEGSR  sr_DatiJob;
050600121031
050700121031         in(e) §AzUte;
050800121031         if NOT %error;
050900121031           in(e) §DatiUte;
051000121031         endif;
051100121031         if %error or RSut = *blank;
051200121031           tibs34r ( tibs34ds );
051300121031           in §AzUte;
051400121031           in §DatiUte;
051500121031         endif;
051600121031
051700121031       ENDSR;
051800121102
051900121102       //--------------------------------------------------------------
052000121102       //?Gestione videata D01.                                        ?
052100121102       //--------------------------------------------------------------
052200121102       BEGSR  sr_GesD01;
052300121102
052400121102         // -?Inizializzazione videata?
052500121102         if  $InzD01 = *on;
052600121102           exsr  sr_InzD01;
052700121102           $InzD01 = *off;
052800121102         endif;
052900121105
053000121105         // -?Abilitazione tasti funzionali?
053100121105         F3Attivo  = *on;
053200121106         F12Attivo = *on;
053300121102
053400121102         // -?Emissione Testata?
053500121102         write  TB1AXT01;
053600121102
053700121102         // -?Emissione videata?
053800121102         exfmt  TB1AXD01;
053900121102
054000121102         reset  ErrMessage;
054100121102         reset  ErrGenerico;
054200121102         clear  V1Dmsg;
054300121102
054400121102         select;
054500121102
054600121102           // -?F3=Fine?
054700121102           when  dsp_aid = c_F03;
054800121102             $Fine = *on;
054900121106
055000121106           // -?F12=Ritorno?
055100121106           when  dsp_aid = c_F12;
055200121106             $Video = 'S1';
055300121102
055400121102           // -?Invio?
055500121102           other;
055600121102             exsr  sr_CtrD01;
055700121102             if  not  ErrGenerico;
055800121102               $Video = 'S1';
055900121102               reset  $InzS01;
056000121102             endif;
056100121102
056200121102         endsl;
056300121102
056400121102       ENDSR;
056500121102
056600121102       //--------------------------------------------------------------
056700121102       //?Inizializzazione videata D01.                                ?
056800121102       //--------------------------------------------------------------
056900121102       BEGSR  sr_InzD01;
057000121102
057100121102         // -?Pulizia videata?
057200121102         clear  TB1AXD01;
057300121102
057400121102       ENDSR;
057500121102
057600121102       //--------------------------------------------------------------
057700121102       //?Controllo dati immessi nella videata D01.                    ?
057800121102       //--------------------------------------------------------------
057900121102       BEGSR  sr_CtrD01;
058000121102
058100121102         // -?Spegnimento degli indicatori di errore?
058200121102         %subst(IndDspF : 50) = *off;
058300121102
058400121102         // -?Esclusione troppe linee di arrivo?
058500121102         if  V1Cdpd = 'S'  and  V1Ceex = 'S'  and  V1Cfed = 'S';
058600121105           V1Dmsg = $Msg(09);
058700121102           PosCurDPD   = *on;
058800121102           ErrMessage  = *on;
058900121102           ErrGenerico = *on;
059000121102           leavesr;
059100121102         endif;
059200121102
059300121102         // -?Pulizia decodifiche LNA x export & Tipi incasso c/assegno?
059400121102         clear  V1Dlna_ds;
059500121102         clear  V1Dtic_ds;
059600121102
059700121102         // -?Controllo Linee di arrivo per export?
059800121106         IF  V1Clna_ds <> *blank  and  V1Clna_ds <> *zero;
059900121105           For  xx = 1  To  %elem(sch_D01lna);
060000121105             if  sch_D01lna(xx) <> *zero;
060100121105               // -?Inserita più di una volta?
060200121105               if  xx  > 1;
060300121105                 yy = %lookup(sch_D01lna(xx) : sch_D01lna);
060400121105                 if  yy < xx;
060500121105                   sch_D01dla(yy) = sch_D01dla(xx);
060600121105                   V1Dmsg = $Msg(10);
060700121105                   PosCurLNA(xx) = *on;
060800121105                   ErrMessage  = *on;
060900121105                   ErrGenerico = *on;
061000121105                   leavesr;
061100121105                 endif;
061200121105               endif;
061300121105               // -?Errata?
061400121106               clear  Og143;
061500121105               k_ORGfil = sch_D01lna(xx);
061600121105               chain  %kds(k01azorg01)  AZORG;
061700121106               if  Not %found(AZORG01L)  or  ORGfva <> *blank;
061800121105                 sch_D01dla(xx) = *all'? ';
061900121105                 V1Dmsg = $Msg(11);
062000121105                 PosCurLNA(xx) = *on;
062100121105                 ErrMessage  = *on;
062200121105                 ErrGenerico = *on;
062300121105                 leavesr;
062400121105               endif;
062500121105               sch_D01dla(xx) = ORGdes;
062600121106               Og143 = ORGde3;
062700121106               // -?Ammesse solo filiali per export?
062800121106               if  Og143.§OGntw <> 'DPD'  and
062900121106                   Og143.§OGntw <> 'EEX'  and
063000121106                   Og143.§OGntw <> 'FED';
063100121106                 V1Dmsg = $Msg(11);
063200121106                 PosCurLNA(xx) = *on;
063300121106                 ErrMessage  = *on;
063400121106                 ErrGenerico = *on;
063500121106                 leavesr;
063600121106               endif;
063700121105               // -?Già esclusa come DPD/EEX/FED?
063800121105               if  (Og143.§OGntw = 'DPD'  and  V1Cdpd = 'S')  or
063900121105                   (Og143.§OGntw = 'EEX'  and  V1Ceex = 'S')  or
064000121105                   (Og143.§OGntw = 'FED'  and  V1Cfed = 'S');
064100121105                 select;
064200121105                   when  Og143.§OGntw = 'DPD'  and  V1Cdpd = 'S';
064300121105                     V1Dmsg = %trimr($Msg(12)) + ' DPD';
064400121105                   when  Og143.§OGntw = 'EEX'  and  V1Ceex = 'S';
064500121105                     V1Dmsg = %trimr($Msg(12)) + ' EuroExpress';
064600121105                   when  Og143.§OGntw = 'FED'  and  V1Cfed = 'S';
064700121105                     V1Dmsg = %trimr($Msg(12)) + ' FedEx';
064800121105                 endsl;
064900121105                 PosCurLNA(xx) = *on;
065000121105                 ErrMessage  = *on;
065100121105                 ErrGenerico = *on;
065200121105                 leavesr;
065300121105               endif;
065400121105             endif;
065500121105           EndFor;
065600121105         ENDIF;
065700121102
065800121102         // -?Controllo Tipi incasso c/assegno?
065900121105         IF  V1Ctic_ds <> *blank;
066000121105           For  xx = 1  To  %elem(sch_D01tic);
066100121105             if  sch_D01tic(xx) <> *blank;
066200121106               // -?Ricerca?
066300121106               if  %scan('?' : sch_D01tic(xx)) > *zero;
066400121106                 clear  sch_D01tic(xx);
066500121106                 PosCurTIC(xx) = *on;
066600121106                 ErrGenerico = *on;
066700121106                 TABEL_Ricerca( '1A' :
066800121106                                *blank :
066900121106                                idElemento :
067000121106                                tastoFunzionaleUscita );
067100121106                 if  tastoFunzionaleUscita = *blank;
067200121106                   if  idElemento = *blank;
067300121106                     sch_D01tic(xx) = c_TIC_blank;
067400121106                   else;
067500121106                     sch_D01tic(xx) = idElemento;
067600121106                   endif;
067700121106                 else;
067800121106                   leavesr;
067900121106                 endif;
068000121106               endif;
068100121105               // -?Inserito più di una volta?
068200121105               if  xx  > 1;
068300121105                 yy = %lookup(sch_D01tic(xx) : sch_D01tic);
068400121105                 if  yy < xx;
068500121105                   sch_D01dti(yy) = sch_D01dti(xx);
068600121105                   V1Dmsg = $Msg(05);
068700121105                   PosCurTIC(xx) = *on;
068800121105                   ErrMessage  = *on;
068900121105                   ErrGenerico = *on;
069000121105                   leavesr;
069100121105                 endif;
069200121105               endif;
069300121105               // -?Errato?
069400121105               if  sch_D01tic(xx) = c_TIC_blank;
069500121105                 clear  ds_TNTBE.TBEke1;
069600121105               else;
069700121105                 ds_TNTBE.TBEke1 = sch_D01tic(xx);
069800121105               endif;
069900121105               clear  ds_TNTBE.TBEke2;
070000121105               if  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
070100121105                                *omit : *omit : *omit :
070200121105                                *omit : *omit :
070300121105                                *omit : *omit : *omit : *omit :
070400121105                                *omit : *omit : *omit : *omit :
070500121105                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
070600121105                              <> *zero   OR
070700121105                              ds_TNTBE.TBEatb <> *blank;
070800121105                 sch_D01dti(xx) = *all'? ';
070900121105                 V1Dmsg = $Msg(06);
071000121105                 PosCurTIC(xx) = *on;
071100121105                 ErrMessage  = *on;
071200121105                 ErrGenerico = *on;
071300121105                 leavesr;
071400121105               endif;
071500121105               ds1A   = ds_TNTBE.TBEuni;
071600121105               sch_D01dti(xx) = §1Ades;
071700121105             endif;
071800121105           EndFor;
071900121105         ENDIF;
072000121102
072100121102       ENDSR;
072200121031
072300121031       //--------------------------------------------------------------
072400121031       //?Gestione subfile S01.                                        ?
072500121031       //--------------------------------------------------------------
072600121031       BEGSR  sr_GesS01;
072700121031
072800121031         // -?Inizializzazione videata?
072900121031         if  $InzS01 = *on;
073000121031           exsr  sr_InzS01;
073100121031           $InzS01 = *off;
073200121031         endif;
073300121106
073400121106         // -?Abilitazione tasti funzionali?
073500121106         F3Attivo  = *on;
073600121106         F6Attivo  = *on;
073700121106         F8Attivo  = *on;
073800121106         F9Attivo  = (S01nrr > *zero);
073900121106         F12Attivo = *off;
074000121106         F17Attivo = *on;
074100121031
074200121031         // -?Emissione Testata & Piede?
074300121031         write  tb1AXt01;
074400121031         write  tb1AXp01;
074500121031
074600121031         // -?Posizionamento cursore?
074700121031         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
074800121031         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
074900121031         //  ?si visualizza la pagina che vedeva l'utente quando ha?
075000121031         //  ?premuto l'ultimo tasto.?
075100121031         if  C1CsrRrn > *zero;
075200121031           C1RcdNbr = C1CsrRrn;
075300121031         else;
075400121031           C1RcdNbr = 1;
075500121031           write  tb1AXs00;            //?(no rec.)?
075600121031         endif;
075700121031
075800121031         // -?Emissione videata?
075900121031         exfmt  tb1AXc01;
076000121031
076100121031         reset  ErrGenerico;
076200121031         reset  ErrMessage;
076300121031         clear  V1Dmsg;
076400121031
076500121031         Select;
076600121031
076700121031           // -?F3=Fine?
076800121031           When  dsp_aid = c_F03;
076900121031             $Fine = *on;
077000121031
077100121031           // -?F5=Rivisualizza?
077200121031           When  dsp_aid = c_F05;
077300121031             $InzS01 = *on;
077400121105
077500121105           // -?F6=Immissione?
077600121105           When  dsp_aid = c_F06;
077700121105             $Video  = 'S2';
077800121105             $InzS02 = *on;
077900121105             //DoW  $Video = 'S2'  and  Not $Fine;
078000121105             //  exsr  sr_GesS02;
078100121105             //EndDo;
078200121031
078300121031           // -?F8=Ordinamento?
078400121031           when  dsp_aid = c_F08;
078500121031             exsr  sr_F08S01;
078600121031
078700121031           // -?F12=Ritorno?
078800121106           //when  dsp_aid = c_F12;
078900121106
079000121106           // -?F17=Parzializzazioni?
079100121106           when  dsp_aid = c_F17;
079200121031             eval  $Video = 'D1';
079300121031
079400121031           // -?Invio?
079500121031           Other;
079600121031             // -?Gestione opzioni?
079700121102             if  S01nrr > *zero;
079800121102               exsr  sr_OpzS01;
079900121102               if  ErrGenerico;
080000121102                 leavesr;
080100121102               endif;
080200121102             endif;
080300121031             // -?Richiesto posizionamento?
080400121031             if  (Not Ord_x_Des   and   C1Clna <> SavC1Clna)   OR
080500121031                 (Ord_x_Des       and   C1Dlna <> SavC1Dlna);
080600121031               $InzS01 = *on;
080700121031             endif;
080800121031
080900121031         endsl;
081000121031
081100121031       ENDSR;
081200121031
081300121031       //--------------------------------------------------------------
081400121031       //?Inizializzazione subfile S01.                                ?
081500121031       //--------------------------------------------------------------
081600121031       BEGSR  sr_InzS01;
081700121031
081800121031         // -?Spegnimento degli indicatori di errore?
081900121031         %subst(IndDspF : 50) = *off;
082000121031
082100121031         // -?Pulizia subfile control?
082200121031         if  Not Ord_x_Des;
082300121031           clear  C1Dlna;
082400121031           reset  SavC1Dlna;
082500121031         else;
082600121031           clear  C1Clna;
082700121031           reset  SavC1Clna;
082800121031         endif;
082900121031
083000121031         // -?Pulizia subfile?
083100121031         SflDsp_N    = *on;
083200121031         SflDspCtl_N = *on;
083300121031         write  tb1AXc01;
083400121031         SflDspCtl_N = *off;
083500121031         SflEnd      = *off;
083600121031
083700121031         clear  C1RcdNbr;
083800121031         clear  C1CsrRrn;
083900121031         clear  S01nrr;
084000121031         clear  V1Dmsg;
084100121031         ErrMessage  = *off;
084200121031         ErrGenerico = *off;
084300121031
084400121031         // -?Caricamento completo dei dati nel subfile?
084500121031         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
084600121031         //  ?l'eventuale ordinamento)?
084700121105         clear  k05tntbe01;
084800121105         k_TBEcod = c_Tab;
084900121105         setll  %kds( k05tntbe01 : 1 )  TNTBE000;
085000121105         reade(N)  %kds( k05tntbe01 : 1 )  TNTBE000;
085100121031         $EoF = (%eof(TNTBE01L));
085200121031         exsr  sr_CaricaS01;
085300121031
085400121031         // -?Impaginazione subfile?
085500121031         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
085600121031         if S01nrr > *zero;
085700121031           C1RcdNbr  = 1;
085800121031           C1CsrRrn  = 1;
085900121031         else;
086000121031           clear C1RcdNbr;
086100121031         endif;
086200121031
086300121031         // -?Ordinamento subfile (se premuto F8)?
086400121031         if  Ord_x_Des   and   S01nrr >  *zero;
086500121031           exsr  sr_SortSfl;
086600121031         endif;
086700121031
086800121031       ENDSR;
086900121031
087000121031       //--------------------------------------------------------------
087100121031       //?Caricamento completo del subfile S01                         ?
087200121031       //--------------------------------------------------------------
087300121031       BEGSR  sr_CaricaS01;
087400121031
087500121031         clear  S01nrr;
087600121031         SflNxtChg = *off;
087700121031
087800121031         // -?Ciclo di caricamento dati da tab. "1AX"?
087900121031         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
088000121031         //  ?l'eventuale ordinamento)?
088100121105         clear  k_ORGfil;
088200121031         DOW  Not $EoF;
088300121031
088400121105           // -?Caricamento tipi incasso c/assegno (già inseriti)?
088500121105           //  ?della LNA export in elaborazione (TBEKE1)?
088600121031           k_TBEke1 = TBEke1;
088700121031           clear  xx;
088800121031           clear  yy;
088900121031           clear  sch_TIC_ds;
089000121031           clear  sch_TIC_ann_ds;
089100121031           clear  tb1AXs01;
089200121031           S1Clna = %subst( TBEke1 : 1 : %len(S1Clna) );
089300121031
089400121102           DoW  Not %eof(TNTBE01L)  and  TBEke1 = S1Clna;
089500121031
089600121031             // -?Selezione singolo record file TNTBE00F?
089700121031             exsr  sr_SelezRec;
089800121031
089900121031             // -?Registrazione singolo record nel subfile?
090000121031             If  $RecOK;
090100121031
090200121031               // ·?Impostazione Tipo incasso c/assegno?
090300121031               xx += 1;
090400121031               if  TBEke2 = *blank;
090500121105                 sch_TIC(xx) = c_TIC_blank;
090600121031               else;
090700121102                 sch_TIC(xx) = %subst( TBEke2 : 1 : %len(S1Ctic1) );
090800121031               endif;
090900121031
091000121031             EndIf;
091100121031
091200121105             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
091300121031
091400121031           EndDo;
091500121105
091600121105           // *?Scrittura del record SE reperito almeno un?
091700121105           //  ?tipo incasso c/assegno NON annullato?
091800121105           If  xx > *zero;
091900121120
092000121120             // -?Indicazione ("*") di più di 10 tipi incasso c/assegno?
092100121120             //  ?per la stessa LNA export?
092200121120             if  xx > 10;
092300121120               S1Cpiu = '+';
092400121120             endif;
092500121031
092600121105             // -?Decodifica LNA export?
092700121105             if  %check( c_Digits : S1Clna ) = *zero;
092800121105               k_ORGfil = %int( S1Clna );
092900121105               chain  %kds(k01azorg01)  AZORG;
093000121105               if  %found(AZORG01L)  and  ORGfva = *blank;
093100121105                 S1Dlna = ORGdes;
093200121105               else;
093300121105                 S1Dlna = *all'? ';
093400121105               endif;
093500121105             endif;
093600121105
093700121105             // -?Scrittura record nel subfile?
093800121105             if  sch_TIC_ds <> *blank;
093900121105               S1Ctic_ds = sch_TIC_ds;
094000121105               S01nrr += 1;
094100121105               write  tb1AXs01;
094200121105             endif;
094300121105
094400121105           EndIf;
094500121031
094600121031           // -?Lettura record della LNA export successiva?
094700121105           setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
094800121105           reade(N)  %kds( k05tntbe01 : 1 )  TNTBE000;
094900121031           $EoF = (%eof(TNTBE01L));
095000121031
095100121031         ENDDO;
095200121105
095300121105         // -?Memorizzazione posizionamento effettuato?
095400121105         if  Not Ord_x_Des;
095500121105           SavC1Clna = C1Clna;
095600121105         else;
095700121105           SavC1Dlna = C1Dlna;
095800121105         endif;
095900121031
096000121031         // -?Visualizzazione del SFL (se ci sono dati)?
096100121031         SflDsp_N = (S01nrr <= *zero);
096200121031
096300121031         // -?Attivazione del SFLEND?
096400121031         SflEnd = ( $EoF );
096500121105
096600121105         // -?Impostazione indicatore x la gestione del posizionamento?
096700121105         //  ?nel subfile?
096800121105         NoRecSfl1 = (S01nrr <= *zero);
096900121031
097000121031         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
097100121031         if  S01nrr > *zero;
097200121031           C1RcdNbr = 1;
097300121031         else;
097400121031           clear  C1RcdNbr;
097500121031         endif;
097600121031
097700121031         C1CsrRrn = C1RcdNbr;
097800121031
097900121031       ENDSR;
098000121031
098100121031       //--------------------------------------------------------------
098200121031       //?Selezione del singolo record letto dalla tabella "1AX".      ?
098300121031       //--------------------------------------------------------------
098400121031       BEGSR  sr_SelezRec;
098500121031
098600121031         reset  $RecOK;
098700121105
098800121105         if  %int( %subst( TBEke1 : 1 : %len(ORGfil) ) ) <> k_ORGfil;
098900121105           clear  Og143;
099000121105           k_ORGfil = %int( %subst( TBEke1 : 1 : %len(ORGfil) ) );
099100121105           chain  %kds(k01azorg01)  AZORG;
099200121105           if  %found(AZORG01L)  and  ORGfva = *blank;
099300121105             Og143 = ORGde3;
099400121105           endif;
099500121105         endif;
099600121031
099700121031         select;
099800121105
099900121105           // -?Esclusione per network?
100000121105           when  (Og143.§OGntw = 'DPD'  and  V1Cdpd = 'S');
100100121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
100200121105             leavesr;
100300121105           when  (Og143.§OGntw = 'EEX'  and  V1Ceex = 'S');
100400121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
100500121105             leavesr;
100600121105           when  (Og143.§OGntw = 'FED'  and  V1Cfed = 'S');
100700121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
100800121105             leavesr;
100900121031
101000121031           // -?Selezione per linea di arrivo?
101100121031           when  V1Clna_ds <> *zero  and
101200121031                 %lookup( %int( %subst( TBEke1 : 1 : %len(V1Clna1) ) )
101300121031                          : sch_D01lna ) = *zero;
101400121105             setgt  %kds( k05tntbe01 : 2 )  TNTBE000;
101500121031             leavesr;
101600121031
101700121031           // -?Selezione per tipo incasso contrassegno?
101800121106           //  ?(*blank <=> "--")?
101900121106           when  V1Ctic_ds <> *blank    AND
102000121106                 ( ( TBEke2  <> *blank  and
102100121106                     %lookup( %subst( TBEke2 : 1 : %len(V1Ctic1) ) :
102200121106                                            sch_D01tic ) = *zero )  OR
102300121106                   ( TBEke2  =  *blank  and
102400121106                     %lookup( c_TIC_blank : sch_D01tic ) = *zero ) );
102500121106             leavesr;
102600121031
102700121031           // -?Omissione records annullati?
102800121031           when  TBEatb <> *blank;
102900121031             yy += 1;
103000121031             if  TBEke2 = *blank;
103100121105               sch_TIC_ann(yy) = c_TIC_blank;
103200121031             else;
103300121102               sch_TIC_ann(yy) = %subst( TBEke2 : 1 : %len(S1Ctic1) );
103400121031             endif;
103500121031             leavesr;
103600121031
103700121031           // -?Richiesto posizionamento per cod. LNA export?
103800121031           //  ?si scartano i record con cod. LNA export inferiore?
103900121031           when  Not Ord_x_Des   and   TBEke1 < %editc( C1Clna : 'X' );
104000121031             leavesr;
104100121031
104200121031           // -?Richiesto posizionamento per descr. LNA export?
104300121031           //  ?si scartano i record con descr. LNA export inferiore?
104400121102           when  Ord_x_Des   and
104500121102                 %subst( TBEuni : 1 : %len(C1Dlna) ) < C1Dlna;
104600121031             leavesr;
104700121031
104800121031         endsl;
104900121031
105000121031         $RecOK = *on;
105100121031
105200121031       ENDSR;
105300121102
105400121102       //--------------------------------------------------------------
105500121102       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
105600121102       //?F8-Cambia ordinamento sfl                                    ?
105700121102       //--------------------------------------------------------------
105800121102       BEGSR  sr_F08S01;
105900121102
106000121102         Ord_x_Des = (Not Ord_x_Des);
106100121102
106200121102         select;
106300121102           // -?Subfile vuoto: nessun record da ordinare?
106400121102           when  S01nrr = *zero;
106500121102           // -?Subfile già posizionato: occorre ricaricarlo?
106600121102           when  C1Clna <> *zero  or  C1Dlna <> *blank;
106700121102             $InzS01 = *on;
106800121102           // -?Altrimenti: basta riordinarlo?
106900121102           other;
107000121102             exsr  sr_SortSfl;
107100121102         endsl;
107200121102
107300121102         if  Not Ord_x_Des;
107400121102           clear  C1Clna;
107500121102           clear  SavC1Clna;
107600121102         else;
107700121102           clear  C1Dlna;
107800121102           clear  SavC1Dlna;
107900121102         endif;
108000121102
108100121102       ENDSR;
108200121102
108300121102       //--------------------------------------------------------------
108400121102       //?Ordinamento subfile                                          ?
108500121102       //?  This subroutine sorts the subfile records.                 ?
108600121102       //--------------------------------------------------------------
108700121102       BEGSR  sr_SortSfl;
108800121102
108900121102         RrnLast  = S01nrr;
109000121102
109100121102         C1RcdNbr = 1;
109200121102         clear  C1CsrRrn;
109300121102         clear  S01nrr;
109400121102         clear  V1Dmsg;
109500121102         ErrMessage  = *off;
109600121102         SflNxtChg   = *off;
109700121102         ErrGenerico = *off;
109800121102         %subst(IndDspF : 50) = *off;
109900121102
110000121102         //?___________________________________________________________?
110100121102         //?Initialize the key fields to sort on.?
110200121102         //?There is one extra field not in the subfile -- Selected --?
110300121102         //?that is added to the record. This field is used to place?
110400121102         //?selected records in front of nonselected records.?
110500121102         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
110600121102
110700121102         clear  QLGKL;
110800121102         clear  QLGSKL;
110900121102         clear  QLGSCB;
111000121102         clear  QLGSCB00;
111100121102
111200121102         // -?Gestione della scelta ordinamento:?
111300121102         //  ?Ordinamento per cod. filiale di arrivo export?
111400121102         If  Ord_x_Des = *off;
111500121102           // -?2 campi chiave?
111600121102           QLGNBRK  = 2;
111700121102           // -?1° campo: cod. filiale di arrivo export (S1Clna)?
111800121102           //            ?a posizone    1,  3 byte, int,  ascending.?
111900121102           QLGSP    = 1;
112000121102           QLGSS    = %size(S1Clna);
112100121102           QLGDT    = Numero;
112200121102           QLGSO    = Ascendente;
112300121102           QLGKL(1) = QLGSKL;
112400121102           // -?2° campo: descr. filiale di arrivo export (S1Dlna)?
112500121102           //            ?a posizone    4, 20 byte, char, ascending.?
112600121102           QLGSP    = 4;
112700121102           QLGSS    = %size(S1Dlna);
112800121102           QLGDT    = Carattere;
112900121102           QLGSO    = Ascendente;
113000121102           QLGKL(2) = QLGSKL;
113100121102
113200121102         //  ?Ordinamento per descr. filiale di arrivo export?
113300121102         Else;
113400121102           // -?2 campi chiave?
113500121102           QLGNBRK  = 2;
113600121102           // -?1° campo: descr. filiale di arrivo export (S1Dlna)?
113700121102           //            ?a posizone    4, 20 byte, char, ascending.?
113800121102           QLGSP    = 4;
113900121102           QLGSS    = %size(S1Dlna);
114000121102           QLGDT    = Carattere;
114100121102           QLGSO    = Ascendente;
114200121102           QLGKL(1) = QLGSKL;
114300121102           // -?2° campo: cod. filiale di arrivo export (S1Clna)?
114400121102           //            ?a posizone    1,  3 byte, int,  ascending.?
114500121102           QLGSP    = 1;
114600121102           QLGSS    = %size(S1Clna);
114700121102           QLGDT    = Numero;
114800121102           QLGSO    = Ascendente;
114900121102           QLGKL(2) = QLGSKL;
115000121102
115100121102         EndIf;
115200121102
115300121102         // -?Load other sort parameters.?
115400121102         QLGLB   = 80 + 16 * MaxKey;
115500121102         QLGRL   = %size(SflRcd) - 1;
115600121102         QLGRT   = 8;
115700121102         QLGOKL  = 80;
115800121102         QLGLKE  = 16;
115900121102         QLGLSS  = 290;
116000121102         // -?Initialize Sort I/O API fields.?
116100121102         QLGRL00 = QLGRL;
116200121102         QLGRC00 = 1;
116300121102         clear  QUSEI;
116400121102         QUSBPRV = %size(QUSEC);
116500121102
116600121102         // -?First step - Initialize the sort routine.?
116700121102         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
116800121102                      SizeList : ReturnSize : QUSEC );
116900121102
117000121102         // -?Next step - Write records to I/O routine.?
117100121102         QLGRT00  = Put;
117200121102         For  Nrr = 1  To  RrnLast;
117300121102           chain  Nrr  tb1AXs01;
117400121102           // -?Solo le righe con Selected = 'Y' sono riordinate,?
117500121102           //  ?quindi per fare un ordinamento di tutte le righe?
117600121102           //  ?metto 'Y' sempre.?
117700121102           Selected = 'Y';
117800121102           clear  QUSEI;
117900121102           QUSBPRV  = %size(QUSEC);
118000121102           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
118100121102                         SizeList : NotUsed : QUSEC );
118200121102         EndFor;
118300121102
118400121102         // -?Next step - Signal end of input, clear subfile for reload.?
118500121102         QLGRT00 = EndPut;
118600121102         clear  QUSEI;
118700121102         QUSBPRV = %size(QUSEC);
118800121102         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
118900121102                       SizeList : NotUsed : QUSEC );
119000121102
119100121102         // -?Pulizia subfile?
119200121102         SflDsp_N    = *on;
119300121102         SflDspCtl_N = *on;
119400121102         write  tb1AXc01;
119500121102         SflDspCtl_N = *off;
119600121102         SflEnd      = *off;
119700121102
119800121102         // -?Final step - Write the records back to the subfile.?
119900121102         QLGRT00  = Get;
120000121105         For  Nrr = 1  To  RrnLast;
120100121102           clear  QUSEI;
120200121102           QUSBPRV = %size(QUSEC);
120300121102           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
120400121102                         QLGRL00  : NotUsed : QUSEC );
120500121102           // -?Ripristino indicatori utilizzati nel sfl rec?
120600121102           SflNxtChg = (S1Copz <> *zero);
120700121102           S01nrr = Nrr;
120800121102           write  tb1AXs01;
120900121102         EndFor;
121000121102
121100121102         C1CsrRrn = 1;
121200121102
121300121102         // -?Visualizzazione del SFL (se ci sono dati)?
121400121102         SflDsp_N = (S01nrr <= *zero);
121500121102
121600121102         // -?Attivazione del SFLEND?
121700121102         SflEnd = $EoF;
121800121102
121900121102       ENDSR;
122000121031
122100121031       //--------------------------------------------------------------
122200121031       //?Caricamento completo del subfile S01                         ?
122300121031       //--------------------------------------------------------------
122400121031       BEGSR  sr_OpzS01;
122500121031
122600121031         clear  SavS01csr;
122700121105
122800121105         // -?Spegnimento degli indicatori di errore?
122900121105         %subst(IndDspF : 50) = *off;
123000121031
123100121031         // -?Ciclo di lettura subfile?
123200121031         readc  tb1AXs01;
123300121031
123400121031         DoW  Not %eof(TNTB1AXD);
123500121031
123600121031           SflNxtChg = *off;
123700121031           %subst(IndDspF : 50) = *off;
123800121031           C1RcdNbr = S01nrr;
123900121031
124000121031           select;
124100121031
124200121031             // -?Opz. 2=Modifica?
124300121031             //  ?Opz. 3=Copia?
124400121031             //  ?Opz. 4=Annullamento/Ripristino?
124500121031             //  ?Opz. 5=Visualizzazione?
124600121031             when  S1Copz = 2  or  S1Copz = 3  or  S1Copz = 4  or
124700121031                   S1Copz = 5;
124800121031               $Video  = 'S2';
124900121031               $InzS02 = *on;
125000121031               DoW  $Video = 'S2'  and  Not $Fine;
125100121031                 exsr  sr_GesS02;
125200121031               EndDo;
125300121031
125400121031             // -?Nessuna opzione?
125500121031             when  S1Copz = *zero;
125600121031
125700121031             // -?Opzione errata?
125800121031             other;
125900121031               ErrGenerico = *on;
126000121031               ErrMessage  = *on;
126100121031               PosCurOPZ   = *on;
126200121031               V1Dmsg = $Msg(01);
126300121031               $Fine  = *off;
126400121031
126500121031           endsl;
126600121031
126700121031           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
126800121031           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
126900121031           if  S1Copz  <> *zero    and
127000121031              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
127100121031             SavS01csr   = C1RcdNbr;
127200121031           endif;
127300121031
127400121031           // -?Aggiornamento sfl?
127500121031           select;
127600121031             when  ErrMessage;
127700121031               SflNxtChg = *on;
127800121031               C1CsrRrn  = C1RcdNbr;
127900121031             when  ErrGenerico;
128000121031               C1CsrRrn  = C1RcdNbr;
128100121031               clear  S1Copz;
128200121031             other;
128300121031               clear  S1Copz;
128400121031           endsl;
128500121031
128600121031           UPDATE  tb1AXs01;
128700121031
128800121031           if  ErrGenerico   or   ErrMessage;
128900121031             leave;
129000121031           endif;
129100121031
129200121031           readc  tb1AXs01;
129300121031
129400121031         ENDDO;
129500121031
129600121031         // -?Riposizionam. cursore sul record contenente la prima opz.?
129700121031         //  ?(se non sono stati rilevati errori)?
129800121031         if  NOT ErrMessage   and   NOT ErrGenerico   and
129900121031             SavS01csr > *zero;
130000121031           C1CsrRrn = SavS01csr;
130100121031         endif;
130200121031
130300121031       ENDSR;
130400121031
130500121031       //--------------------------------------------------------------
130600121031       //?Gestione subfile S02.                                        ?
130700121031       //--------------------------------------------------------------
130800121031       BEGSR  sr_GesS02;
130900121031
131000121031         // -?Inizializzazione videata?
131100121031         if  $InzS02 = *on;
131200121031           exsr  sr_InzS02;
131300121031           $InzS02 = *off;
131400121031         endif;
131500121031
131600121031         // -?Emissione Testata & Piede?
131700121031         write  tb1AXt01;
131800121031         write  tb1AXp02;
131900121031
132000121031         // -?Posizionamento cursore?
132100121031         //  ?C2CsrRrn contiene il numero di riga del subfile su cui?
132200121031         //  ?era posizionato il cursore; impostandolo in C2RcdNbr?
132300121031         //  ?si visualizza la pagina che vedeva l'utente quando ha?
132400121031         //  ?premuto l'ultimo tasto.?
132500121031         if  C2CsrRrn > *zero;
132600121031           C2RcdNbr = C2CsrRrn;
132700121031         endif;
132800121031
132900121031         // -?Emissione videata?
133000121031         //  ?(protetta se visualizzazione o annullamento)?
133100121031         if  Not $Immissione  AND
133200121031            (S1Copz = 5       or
133300121105             S1Copz = 4);
133400121105            //(S1Copz = 4  and  TBEatb = *blank));
133500121031           write  tb1AXc02;
133600121031           exfmt  Protect;
133700121031         else;
133800121031           exfmt  tb1AXc02;
133900121031         endif;
134000121031
134100121031         reset  ErrGenerico;
134200121031         reset  ErrMessage;
134300121031         clear  V1Dmsg;
134400121031
134500121031         Select;
134600121031
134700121031           // -?F3=Fine?
134800121031           When  dsp_aid = c_F03;
134900121031             unlock  TNTBE01L;
135000121031             //$Fine  = *on;
135100121031             $Video = 'S1';
135200121031             ErrGenerico = *on;
135300121031
135400121031           // -?F12=Ritorno?
135500121031           When  dsp_aid = c_F12;
135600121031             unlock  TNTBE01L;
135700121031             $Video = 'S1';
135800121031
135900121105           // -?Invio, F6=Conferma?
136000121031           Other;
136100121031             // -?Controlli eseguiti NON se Annullamento?
136200121105             if  $Immissione   or  S1Copz <> 4;
136300121105                 //or  (S1Copz = 4  and  TBEatb <> *blank);
136400121031               exsr  sr_CtrC02;
136500121031             endif;
136600121031             if  ErrGenerico;
136700121031               leavesr;
136800121031             endif;
136900121031             // -?Aggiornamento?
137000121031             if  dsp_aid = c_F06;
137100121031               $Video  = 'W1';
137200121031               $InzW01 = *on;
137300121031               DoW  $Video = 'W1';
137400121031                 exsr  sr_GesW01;
137500121031               EndDo;
137600121031             endif;
137700121031
137800121031         EndSl;
137900121031
138000121031       ENDSR;
138100121031
138200121031       //--------------------------------------------------------------
138300121031       //?Inizializzazione subfile S02.                                ?
138400121031       //--------------------------------------------------------------
138500121031       BEGSR  sr_InzS02;
138600121031
138700121031         // -?Spegnimento degli indicatori di errore?
138800121031         %subst(IndDspF : 50) = *off;
138900121031
139000121031         // -?Reperimento dati dal file?
139100121031         clear  xx;
139200121031         clear  sch_TIC_ds;
139300121105         clear  sch_TICd_ds;
139400121105         IF  dsp_aid <> c_F06;
139500121031           clear  k05tntbe01;
139600121031           k_TBEcod = c_Tab;
139700121031           k_TBEke1 = S1Clna;
139800121031           setll  %kds( k05tntbe01 : 2 )  TNTBE000;
139900121105           if  S1Copz = 3  or  S1Copz = 5;
140000121105             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
140100121105           else;
140200121105             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
140300121105           endif;
140400121031           DoW  Not %eof(TNTBE01L);
140500121031             if  TBEatb = *blank;
140600121031               xx += 1;
140700121031               if  TBEke2 = *blank;
140800121105                 sch_TIC(xx) = c_TIC_blank;
140900121031               else;
141000121102                 sch_TIC(xx) = %subst( TBEke2 : 1 : %len(S1Ctic1) );
141100121031               endif;
141200121031             endif;
141300121105             if  S1Copz = 3  or  S1Copz = 5;
141400121105               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
141500121105             else;
141600121105               reade  %kds( k05tntbe01 : 2 )  TNTBE000;
141700121105             endif;
141800121031           EndDo;
141900121031         ENDIF;
142000121031
142100121031         // -?Pulizia subfile-control?
142200121031         clear  tb1AXc02;
142300121031
142400121031         // -?Pulizia subfile?
142500121031         Sfl2Dsp_N    = *on;
142600121031         Sfl2DspCtl_N = *on;
142700121031         write  tb1AXc02;
142800121031         Sfl2DspCtl_N = *off;
142900121031         Sfl2End      = *off;
143000121031
143100121031         clear  C2RcdNbr;
143200121031         clear  C2CsrRrn;
143300121031         clear  S02nrr;
143400121031         clear  V1Dmsg;
143500121031         ErrMessage  = *off;
143600121031         ErrGenerico = *off;
143700121031
143800121031         // -?Caricamento dei dati nel subfile-control?
143900121031         select;
144000121105            when  dsp_aid = c_F06;
144100121031              V2Topz = '  INSERIMENTO  ';
144200121031            when  S1Copz  = 2;
144300121031              V2Topz = '   MODIFICA    ';
144400121031            when  S1Copz  = 3;
144500121031              V2Topz = '     COPIA     ';
144600121031            when  S1Copz  = 4;
144700121031              V2Topz = ' ANNULLAMENTO  ';
144800121031            //when  S1Copz  = 4  and  TBEatb <> *blank;
144900121031            //  V2Topz = '  RIPRISTINO   ';
145000121031            when  S1Copz  = 5;
145100121031              V2Topz = 'VISUALIZZAZIONE';
145200121031         endsl;
145300121031
145400121031         // ·?Protezione campi chiave SE NON inserimento?
145500121105         $Immissione = (dsp_aid = c_F06);
145600121105         ProtectKEY  = (dsp_aid <> c_F06  and  S1Copz <> 3);
145700121031
145800121031         // -?Impostazione LNA export  e?
145900121031         //  ?Caricamento dei tipi incasso c/assrgno?
146000121031         //  ?(SE NON immissione o copia)?
146100121105         If  dsp_aid <> c_F06  and  S1Copz <> 3;
146200121031
146300121031           // -?Filiale di arrivo per export?
146400121031           C02lna = %int( %trim(TBEke1) );
146500121031           k_ORGfil = C02lna;
146600121031           chain  %kds(k01azorg01)  AZORG;
146700121031           if  %found(AZORG01L);
146800121031             C02lnaD = ORGdes;
146900121031           endif;
147000121031
147100121031         EndIf;
147200121105
147300121108         // -?Note: indicazione di "  " visualizzata come "--"?
147400121105         clear  ds_TNTBE.TBEke1;
147500121105         if  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
147600121105                          *omit : *omit : *omit :
147700121105                          *omit : *omit :
147800121105                          *omit : *omit : *omit : *omit :
147900121105                          *omit : *omit : *omit : *omit :
148000121105                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
148100121105                        = *zero   AND
148200121105                        ds_TNTBE.TBEatb = *blank;
148300121105           ds1A   = ds_TNTBE.TBEuni;
148400121113           C02note = '("--" = blank - ' + %trim(§1Ades) +')';
148500121105         endif;
148600121031
148700121031         // -?Tipi incasso c/assegno?
148800121031         For  xx = 1  To  c_MaxTIC;
148900121031
149000121031           clear  tb1AXs02;
149100121031
149200121031           If  sch_TIC(xx) <> *blank;
149300121031
149400121031             Sfl2NxtChg = *on;
149500121031             S2Ctic = sch_TIC(xx);
149600121105             if  S2Ctic = c_TIC_blank;
149700121105               clear  ds_TNTBE.TBEke1;
149800121105             else;
149900121105               ds_TNTBE.TBEke1 = S2Ctic;
150000121105             endif;
150100121031             clear  ds_TNTBE.TBEke2;
150200121031             if  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
150300121031                              *omit : *omit : *omit :
150400121031                              *omit : *omit :
150500121031                              *omit : *omit : *omit : *omit :
150600121031                              *omit : *omit : *omit : *omit :
150700121031                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
150800121031                            = *zero   AND
150900121031                            ds_TNTBE.TBEatb = *blank;
151000121031               ds1A   = ds_TNTBE.TBEuni;
151100121031               S2Dtic = §1Ades;
151200121031             else;
151300121031               S2Dtic = *all'? ';
151400121031             endif;
151500121031
151600121031           EndIf;
151700121031
151800121031           // -?Scrittura record nel subfile?
151900121031           S02nrr += 1;
152000121031           write  tb1AXs02;
152100121031
152200121031         EndFor;
152300121031
152400121031         // -?Posizionamento cursore sulla linea di arrivo export?
152500121031         //  ?(SE immissione)?
152600121105         PosCurLNAc2 = (dsp_aid = c_F06  or  S1Copz = 3);
152700121031
152800121031         // -?Visualizzazione del SFL  (SE ci sono dati)?
152900121031         Sfl2Dsp_N = (S02nrr <= *zero);
153000121031
153100121031         // -?Attivazione del SFLEND?
153200121031         Sfl2End = *on;
153300121031
153400121031         // -?Impaginazione subfile?
153500121031         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
153600121031         if S02nrr > *zero;
153700121031           C2RcdNbr  = 1;
153800121031           C2CsrRrn  = 1;
153900121031         else;
154000121031           clear C2RcdNbr;
154100121031         endif;
154200121031
154300121031         // -?Abilitazione tasti funzionali condizionati?
154400121102         F3Attivo  = *on;
154500121031         F6Attivo  = ($Immissione  or  S1Copz <> 5);
154600121102         F8Attivo  = (S01nrr > 1);
154700121102         F9Attivo  = (S01nrr > *zero);
154800121102         F12Attivo = *on;
154900121031
155000121031       ENDSR;
155100121031
155200121031       //--------------------------------------------------------------
155300121031       //?Controllo dati nella videata C02/S02.                        ?
155400121031       //--------------------------------------------------------------
155500121031       BEGSR  sr_CtrC02;
155600121031
155700121031         // -?Spegnimento degli indicatori di errore?
155800121031         %subst(IndDspF : 50) = *off;
155900121031
156000121031         // -?Controllo linea di arrivo export?
156100121031         // ·?Obbligatoria?
156200121031         if  C02lna = *zero;
156300121031           ErrGenerico = *on;
156400121031           ErrMessage  = *on;
156500121105           PosCurLNAc2 = *on;
156600121031           V1Dmsg = $Msg(03);
156700121031           leavesr;
156800121031         endif;
156900121031         // ·?Errata?
157000121102         k_ORGfil = C02lna;
157100121031         chain  %kds(k01azorg01)  AZORG;
157200121031         if  Not %found(AZORG01L)   or   ORGfva <> *blank   or
157300121031             (ORGfag <> 'F'   and   ORGfag <> 'A');
157400121031           ErrGenerico = *on;
157500121031           ErrMessage  = *on;
157600121105           PosCurLNAc2 = *on;
157700121031           V1Dmsg = $Msg(03);
157800121031           leavesr;
157900121031         endif;
158000121031         C02lnaD = ORGdes;
158100121106         Og143 = ORGde3;
158200121106         if  Og143.§OGntw <> 'DPD'  and
158300121106             Og143.§OGntw <> 'EEX'  and
158400121106             Og143.§OGntw <> 'FED';
158500121106           ErrGenerico = *on;
158600121106           ErrMessage  = *on;
158700121106           PosCurLNAc2 = *on;
158800121106           V1Dmsg = $Msg(03);
158900121106           leavesr;
159000121106         endif;
159100121031
159200121031         // -?Controllo SE chiave duplicata (SE immissione)?
159300121105         If  $Immissione  or  S1Copz = 3;
159400121031           clear  k05tntbe01;
159500121031           k_TBEcod = c_Tab;
159600121031           k_TBEke1 = %editc( C02lna : 'X' );
159700121031           setll  %kds( k05tntbe01 : 2 )  TNTBE000;
159800121105           reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
159900121105           dow  Not %eof(TNTBE01L);
160000121105             if  TBEatb = *blank;
160100121105               ErrGenerico = *on;
160200121105               ErrMessage  = *on;
160300121105               PosCurLNAc2 = *on;
160400121105               V1Dmsg = $Msg(04);
160500121105               leavesr;
160600121105             endif;
160700121105             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
160800121105           enddo;
160900121031         EndIf;
161000121031
161100121031         // -?Controllo tipi incasso c/assegno nel subfile S02?
161200121031         clear  sch_TIC;
161300121105         clear  sch_TICd;
161400121031         readc  tb1AXs02;
161500121031
161600121031         DoW  Not %eof(TNTB1AXD);
161700121031
161800121031           exsr  sr_CtrS02;
161900121031
162000121102           // ·?Aggiornamento sfl?
162100121031           select;
162200121031             when  ErrMessage;
162300121031               Sfl2NxtChg = *on;
162400121031               C2CsrRrn  = C2RcdNbr;
162500121031             when  ErrGenerico;
162600121105               Sfl2NxtChg = *on;
162700121031               C2CsrRrn  = C2RcdNbr;
162800121106             when  S2Ctic <> *blank;
162900121106               Sfl2NxtChg = *on;
163000121106               //C2CsrRrn  = C2RcdNbr;
163100121031           endsl;
163200121031           UPDATE  tb1AXs02;
163300121031
163400121031           if  ErrGenerico   or   ErrMessage;
163500121108             leavesr;
163600121031           endif;
163700121031
163800121031           readc  tb1AXs02;
163900121031
164000121031         EndDo;
164100121108
164200121108         C2RcdNbr  = 1;
164300121108         C2CsrRrn  = C2RcdNbr;
164400121105
164500121105         // -?Almeno un Tipo incasso c/assegno obbligatorio?
164600121105         if  sch_TIC_ds = *blank;
164700121105           ErrGenerico = *on;
164800121105           ErrMessage  = *on;
164900121105           PosCurTICs2 = *on;
165000121105           V1Dmsg = $Msg(07);
165100121105           leavesr;
165200121105         endif;
165300121031
165400121031       ENDSR;
165500121031
165600121031       //--------------------------------------------------------------
165700121031       //?Controllo dati nel subfile S02.                              ?
165800121031       //--------------------------------------------------------------
165900121031       BEGSR  sr_CtrS02;
166000121105
166100121105         C2RcdNbr = S02nrr;
166200121105
166300121105         // -?Spegnimento degli indicatori di errore?
166400121105         %subst(IndDspF : 50) = *off;
166500121105
166600121031         clear  S2Dtic;
166700121105
166800121105         // -?Tipo incasso c/assegno: ricerca?
166900121105         If  %scan('?' : S2Ctic) > *zero;
167000121105
167100121105           clear  S2Ctic;
167200121105           TABEL_Ricerca( '1A' :
167300121105                          *blank :
167400121105                          idElemento :
167500121105                          tastoFunzionaleUscita );
167600121105           if  tastoFunzionaleUscita = *blank;
167700121105             if  idElemento = *blank;
167800121105               S2Ctic = c_TIC_blank;
167900121105             else;
168000121105               S2Ctic = idElemento;
168100121105             endif;
168200121105           else;
168300121105             ErrGenerico = *on;
168400121105           endif;
168500121105           PosCurTICs2 = *on;
168600121105
168700121105         EndIf;
168800121105
168900121105         // -?Tipo incasso c/assegno: controllo?
169000121105         //If  S2Ctic <> *blank  and  S2Ctic <> *all'9';
169100121105         If  S2Ctic <> *blank;
169200121105
169300121105           // ·?Ripetuto nel subfile?
169400121105           if  %lookup( S2Ctic : sch_TIC ) > *zero;
169500121105             ErrGenerico = *on;
169600121105             ErrMessage  = *on;
169700121105             PosCurTICs2 = *on;
169800121105             V1Dmsg = $Msg(05);
169900121105             leavesr;
170000121105           endif;
170100121031
170200121105           // ·?Controllo/Decodifica?
170300121105           if  S2Ctic = c_TIC_blank;
170400121105             clear  ds_TNTBE.TBEke1;
170500121105           else;
170600121105             ds_TNTBE.TBEke1 = S2Ctic;
170700121105           endif;
170800121105           clear  ds_TNTBE.TBEke2;
170900121105           If  getTabella ( c_Tabel : '1A' : ds_TNTBE.TBEke1 :
171000121105                            *omit : *omit : *omit :
171100121105                            *omit : *omit :
171200121105                            *omit : *omit : *omit : *omit :
171300121105                            *omit : *omit : *omit : *omit :
171400121105                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
171500121105                          = *zero   AND
171600121105                          ds_TNTBE.TBEatb = *blank;
171700121105             ds1A   = ds_TNTBE.TBEuni;
171800121105             S2Dtic = §1Ades;
171900121105           Else;
172000121105             S2Dtic = *all'? ';
172100121105             ErrGenerico = *on;
172200121105             ErrMessage  = *on;
172300121105             PosCurTICs2 = *on;
172400121105             V1Dmsg = $Msg(06);
172500121105             leavesr;
172600121105           EndIf;
172700121105
172800121105           // ·?Memorizzazione Tipo incasso c/assegno in schiera?
172900121105           //  ?(SE corretto)?
173000121105           xx = %lookup( *blank : sch_TIC);
173100121105           if  xx > *zero;
173200121105             sch_TIC(xx)  = S2Ctic;
173300121105             sch_TICd(xx) = S2Dtic;
173400121105           endif;
173500121031
173600121105         EndIf;
173700121031
173800121031       ENDSR;
173900121031
174000121031       //--------------------------------------------------------------
174100121031       //?Gestione videata W01 (dati relativi alla trasmissione).      ?
174200121031       //--------------------------------------------------------------
174300121031       BEGSR  sr_GesW01;
174400121031
174500121031         // -?Inizializzazione videata?
174600121031         if  $InzW01 = *on;
174700121031           exsr  sr_InzW01;
174800121031           $InzW01 = *off;
174900121031         endif;
175000121031
175100121031         // -?Emissione window?
175200121031         IF  TBXftt = 'S';
175300121031
175400121031           exfmt  tb1AXw01;
175500121031
175600121031           reset  ErrGenerico;
175700121031           reset  ErrMessage;
175800121031           clear  V1Dmsg;
175900121031
176000121031           Select;
176100121031
176200121031             // -?F12=Ritorno?
176300121031             When  dsp_aid = c_F12;
176400121031               $Video = 'S2';
176500121031               leavesr;
176600121031
176700121031             // -?Invio?
176800121031             When  dsp_aid <> c_F06;
176900121031               leavesr;
177000121031
177100121031           EndSl;
177200121031
177300121031         ENDIF;
177400121031
177500121031         // -?F6=Aggiornamento TNTBE00F?
177600121031         //  ?prima annulla TUTTE le tab."1AX" con la LNA export in KE1,?
177700121031         //  ?poi   vi registra TUTTI i tipi incasso del subfile.?
177800121105         clear  k05tntbe01;
177900121105         k_TBEcod = c_Tab;
178000121105         k_TBEke1 = %editc( C02lna : 'X' );
178100121102         exsr  sr_AnnullTNTBE;
178200121105         IF  $Immissione  or  S1Copz <> 4;
178300121105           For  xx = 1  To  %elem(sch_TIC);
178400121105             if  sch_TIC(xx) <> *blank;
178500121105               exsr  sr_UpdTNTBE;
178600121105               if  ErrGenerico;
178700121105                 leave;
178800121105               endif;
178900121105             endif;
179000121105           EndFor;
179100121105         ENDIF;
179200121031
179300121031         If  ErrGenerico = *on;
179400121031           // -?Rilevato errore in fase di aggiornamento?
179500121031           leavesr;
179600121031         Else;
179700121102           // -?SubFile S01 altrimenti?
179800121031           $Video  = 'S1';
179900121105           $InzS01 = ($Immissione  or  S1Copz = 3  or  S1Copz = 4);
180000121102           if  not  $InzS01;
180100121102             S1Ctic_ds = sch_TIC_ds;
180200121031           endif;
180300121031         EndIf;
180400121031
180500121031       ENDSR;
180600121031
180700121031       //--------------------------------------------------------------
180800121031       //?Inizializzazione videata W01                                 ?
180900121031       //--------------------------------------------------------------
181000121031       BEGSR  sr_InzW01;
181100121031
181200121031         clear tb1AXw01;
181300121031
181400121031         select;
181500121031           // -?Opz.3=Copia?
181600121031           when  S1Copz = 3;
181700121031             W1ftt = TBXftt;
181800121031           // -?Opz.4=Ripristino?
181900121031           //when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = 'A';
182000121031           //  W1ftt = TBEftt;
182100121031           // -?Opz.4=Annullamento?
182200121031           when  S1Copz = 4;
182300121031             W1ftt = TBEftt;
182400121031           // -?Opz.2=Modifica?
182500121031           when  S1Copz = 2;
182600121031             W1ftt = TBEftt;
182700121105           // -?F6=Immissione?
182800121031           when  $Immissione;
182900121031             W1ftt = TBXftt;
183000121031         endsl;
183100121031
183200121031         // -?Se NON immissione:?
183300121031         //  ?visualizza i dati relativi all'ultima trasmissione?
183400121031         if  %found(TNTBE01L);
183500121031           W1flt  = TBEflt;
183600121031           W1ftr  = TBEftr;
183700121031           if  TBEdtr <> *zero;
183800121031             wData_Eur = %date( TBEdtr : *iso );
183900121031             W1dtr     = %dec( wData_Eur );
184000121031           endif;
184100121031         endif;
184200121031
184300121031       ENDSR;
184400121031
184500121031       //--------------------------------------------------------------
184600121031       //?Annullamento record gia inseriti in TNTBE00F (tab. "1AX").   ?
184700121031       //--------------------------------------------------------------
184800121031       BEGSR  sr_AnnullTNTBE;
184900121031
185000121105         //clear  k05tntbe01;                   ?(già fatto)?
185100121105         //k_TBEcod = c_Tab;                    ?(già così)?
185200121105         //k_TBEke1 = %editc( C02lna : 'X' );   ?(già così)?
185300121031         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
185400121031         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
185500121031
185600121031         DoW  not %eof(TNTBE01L);
185700121105
185800121105           If  TBEatb <> 'A';
185900121031
186000121105             TBEftt = W1ftt;
186100121105             TBEflt = W1flt;
186200121105             clear TBEftr;
186300121105             TBEatb = 'A';
186400121105             //_______________
186500121105             UPDATE  TNTBE000;
186600121105             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
186700121105
186800121105           EndIf;
186900121102
187000121031           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
187100121031
187200121031         EndDo;
187300121031
187400121031       ENDSR;
187500121031
187600121031       //--------------------------------------------------------------
187700121031       //?Aggiornamento singolo record in TNTBE00F (tab. "1AX").       ?
187800121031       //--------------------------------------------------------------
187900121031       BEGSR  sr_UpdTNTBE;
188000121031
188100121102         // -?Aggancio record da aggiornare/inserire/annullare?
188200121031         //clear  k05tntbe01;                   ?(già fatto)?
188300121031         //k_TBEcod = c_Tab;                    ?(già così)?
188400121031         //k_TBEke1 = %editc( C02lna : 'X' );   ?(già così)?
188500121105         if  sch_TIC(xx) = c_TIC_blank;
188600121031           clear  k_TBEke2;
188700121031         else;
188800121105           k_TBEke2 = sch_TIC(xx);
188900121031         endif;
189000121031         chain  %kds(k05tntbe01)  TNTBE000;
189100121031
189200121031         // -?Impostazione dei campi chiave  SE immissione?
189300121031         if  NOT %found(TNTBE01L);
189400121031           clear  TNTBE000;
189500121031           TBEcod = k_TBEcod;
189600121031           TBEke1 = k_TBEke1;
189700121031           TBEke2 = k_TBEke2;
189800121031         endif;
189900121031
190000121031         // -?Impostazione dati (se NON annullamento)?
190100121106         //  ?(NON potrà MAI essere un annullamento:?
190200121106         //  ? tutti i rec. della LNA sono GIÀ stati annullati)?
190300121102         //if  S1Copz <> 4;
190400121031           TBEapl = TBXapl;
190500121105           TBEuni = C02lnaD + sch_TICd(xx);
190600121102         //endif;
190700121031
190800121031         TBEftt = W1ftt;
190900121031         TBEflt = W1flt;
191000121031         clear TBEftr;
191100121031         //clear TBEdtr;
191200121031
191300121102         select;
191400121102           when  $Immissione;          //?- inserimento?
191500121102             //clear  TBEatb;         ?(già così)?
191600121102           when  S1Copz <> 4;          //?- modifica/copia?
191700121102             clear  TBEatb;
191800121102           other;                      //?- annullamento?
191900121102             //TBEatb = 'A';          ?(NON potrà MAI essere annullam.)?
192000121102         endsl;
192100121031
192200121031         IF  NOT  %found(TNTBE01L);
192300121031           //_________________
192400121031           WRITE(e)  TNTBE000;
192500121031           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
192600121031           if  %error;
192700121031             ErrGenerico = *on;
192800121031             ErrMessage  = *on;
192900121105             PosCurTICs2 = *on;
193000121105             V1Dmsg = %trimr($Msg(08)) + S2Ctic + '"';
193100121031             leavesr;
193200121031           endif;
193300121031         ELSE;
193400121031           //_______________
193500121031           UPDATE  TNTBE000;
193600121031           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193700121031         ENDIF;
193800121031
193900121031       ENDSR;
194000121031
194100121031       //--------------------------------------------------------------
194200121031       //?Operazioni finali.                                           ?
194300121031       //--------------------------------------------------------------
194400121031       BEGSR  sr_RoutEnd;
194500121106
194600121106         // -?Chiusura funzioni precedentemente aperte?
194700121106         cloTabella ( c_Tabel );
194800121031
194900121106         // -?Chiusura *pgm?
195000121031         return;
195100121031
195200121031       ENDSR;
195300121031
195400121031      /end-free
195500121031
195600121031** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
195700121031Opzione errata                                                                  1
195800121031Dati annullati: usare l'opzione 4 per ripristinarli e modificarli               2
195900121106Linea di arrivo EXPORT errata o mancante                                        3
196000121031Linea di arrivo export già presente in tabella                                  4
196100121105Tipo incasso c/assegno già indicato                                             5
196200121105Tipo incasso c/assegno errato                                                   6
196300121105Specificare almeno un tipo incasso c/assegno                                    7
196400121105Rilevato errore in fase di registrazione tipo incasso c/assegno "               8
196500121105Lasciare inclusa almeno un tipo di linee di arrivo (DPD, EuroExpress o FedEx)   9
196600121105Linea di arrivo già indicata                                                   10
196700121106Linea di arrivo EXPORT errata                                                  11
196800121106Linea di arrivo export esclusa come                                            12
