000100101220       //==============================================================
000200101220       //?  STORE PROCEDURE - DATI PER IMMAGINI LDV (TERMINALINI AUT)  ?
000300130411       //?                    (ANCHE RIGENERAZIONE)                    ?
000400170307       //==============================================================
000500170307       //?                     COPIA DI  "FISP30R"                     ?
000600101220       //==============================================================
000700101220       //
000800101220       //? QUESTO PROGRAMMA È "LEGATO"? A "FNLSB6R" (STAMPA LDV A4).  ?
000900130214       //? VERIFICARE SE LA MODIFICA E' DA APPORTARE ANCHE AD ESSO !!  ?
001000101220       //
001100130924       //ATTENZIONE:?I CAMPI NEL RECORD LSB6LDV RISULTANO IMPOSTATI
001200101220       //             NELLE VARIE STRUTTURE DATI FISP30nnDS - TUTTI
001300101220       //             ALFANUMERICI !
001400130924       //
001500130924       //NOTA BENE (a):?Valori previsti nel parm di output RISULTATO:?
001600130924       //               ?· 1) "OK" => Generare l'immagine             ?
001700130924       //               ?· 2) "NO" => Ripetere la richiesta           ?
001800130924       //               ?· 3) "XX" => NON ripetere la richiesta       ?
001900101220       //
002000130924       //NOTA BENE (b):?> Cod. cliente 0501331 => FedEx
002100150430       //                  cod. fissato nella costante "c_CliFedEx"
002200150430       //                > Codice cliente 0506514 => FedEx (2)
002300150430       //                  codice fissato nella costante "c_CliFedEx2"
002400130924       //                > Cod. cliente 0201515 => PraOil oleodotti
002500150430       //                  cod. fissato nella costante "c_CliPraOil"
002600130924       //                > Cod. cliente 0504450 => TTL
002700130924       //                  cod. fissato nella costante "c_CliTTL"
002800130924       //                > Cod. cliente 0632910 => Consorzio Lotterie Naz
002900130924       //                  cod. fissato nella costante "c_CliCLN"
003000140513       //                > Cod. cliente 0017093 => Lotterie Nazionali
003100140513       //                       (gemello di backup su Bologna)
003200140513       //                  cod. fissato nella costante "c_CliLN_2"
003300130924       //                > Cod. cliente 0634923 => S G Soluzioni Logist.
003400130924       //                  cod. fissato nella costante "c_CliSG "
003500130924       //                > Cod. cliente 0635042 => S G Soluzioni Logist.
003600130924       //                  cod. fissato nella costante "c_CliSG_2"
003700101220       //
003800101220       //==============================================================
003900101213
004000101213       //--------------------------------------------------------------
004100101213       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
004200101213       //--------------------------------------------------------------
004300101213
004400130411     /*PRM  dbgview(*source)
004500130411     /*PRM  commit(*NONE)
004600130411     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
004700130411     /*CMD         ovrscope(*calllvl)
004800130411     /*END  dltovr file(TITAS30C) lvl(*)
004900101213     /*END
005000101213
005100101213       //--------------------------------------------------------------
005200101213       //?Specifiche di controllo.                                     ?
005300101213       //--------------------------------------------------------------
005400080430
005500080430     h decedit('0,') datedit(*dmy/) option(*nodebugio)
005600080718     h dftactgrp(*no) actgrp(*caller)
005700080430
005800101213       //--------------------------------------------------------------
005900101213       //?Dichiarazione file.                                          ?
006000101213       //--------------------------------------------------------------
006100080430
006200101213       // -?Tabelle?
006300101213     fTABEL00F  if   e           k disk    usropn
006400101213
006500101213       // -?Organigramma di filiale?
006600101213     fAZORG01L  if   e           k disk    usropn
006700130411
006800130411       // -?Distinte delle cui LdV ri-generare le immagini?
006900130422     fFIIRD11L  if   e           k disk
007000130411
007100130411       // -?Dati per LdV (sede)?
007200130411     fTITAS30C  if   e           k disk    usropn
007300130411     f                                     extfile(wLibFileTITAS)
007400101213
007500101213       // -?Dati per LdV arrivo?
007600070305     fFNARB01L  if   e           k disk    usropn
007700070306     fFNART01L  if   e           k disk    usropn
007800131206     fFNEVB04L  if   e           k disk    usropn
007900140303     fFNEVB22L  if   e           k disk    usropn
008000080710     fFIDST01L  if   e           k disk    usropn
008100101213
008200101213       // -?Altri dati comuni?
008300090619     fFNLBL01L  if   e           k disk    usropn
008400081030     fFIARS01L  if   e           k disk    usropn
008500070305     fFIAR401L  if   e           k disk    usropn
008600030515     fFIAR501L  if   e           k disk    usropn
008700070305     fFIAR601L  if   e           k disk    usropn
008800070305     fFIAR701L  if   e           k disk    usropn
008900070305     fFIAR901L  if   e           k disk    usropn
009000170517     fTFNTC01L  if   e           k disk    usropn
009100171117
009200171117       // -?Bolle: estensione DPD?
009300171117     fFIPND01L  if   e           k disk    usropn
009400110117
009500131017       // -?Dati per LdV partenza (solo per bolla mamma - cliente CLN)?
009600110117     fFNBLP01L  if   e           k disk    usropn
009700101213
009800101207       // -?O.R.M.?
009900100517     fFNORM01L  if   e           k disk    usropn
010000101213
010100110309       // -?File LOG spedizioni elaborate per immagine LdV?
010200110202     fFIILV01L  Uf A e           k disk    usropn
010300130227
010400130227       // -?Spool da inviare via e-mail:?
010500130227       //   ·?Corpo del messaggio di posta elettronica?
010600130227     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOE)
010700101213
010800101207       // -?Printer File  (se richiesta bozza in stampa)?
010900090210     fQSYSPRT   o    f  132        printer usropn  oflind(*inOF)
011000030515
011100101213       //--------------------------------------------------------------
011200101213       //?Definizione costanti.                                        ?
011300101213       //--------------------------------------------------------------
011400030515
011500110117     d c_DateMax_A     c                   const('20391231')
011600170517       // -?Capoconto clienti?
011700170517     d c_Kci           c                   const(0151)
011800131008       // -?Costanti in output?
011900110117     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
012000030515     d                                     NON SEQUENZIALI')
012100110117     d c_CoOr          c                   const('COLLI ORIGINALI     -
012200030717     d                                     N. ')
012300110117     d c_PMxA          c                   const('PROMEMORIA PER L''AU-
012400070315     d                                     TOTRASPORTATORE')
012500101213       // -?Codice cliente "FedEx"?
012600150430     d c_CliFedEx      c                   const(0501331)
012700150430     d c_CliFedEx2     c                   const(0506514)
012800101213       // -?Codice cliente "PraOil oleodotti italiani"?
012900150430     d c_CliPraOil     c                   const(0201515)
013000101213       // -?Codice cliente "TTL"?
013100091116     d c_CliTTL        c                   const(0504450)
013200101213       // -?Codice cliente "Lotterie Nazionali - Gratta & Vinci"?
013300090619     d c_CliCLN        c                   const(0632910)
013400140513     d c_CliLN_2       c                   const(0017093)
013500101213       // -?Codice cliente "S G Soluzioni Logistiche"?
013600090619     d c_CliSG         c                   const(0634923)
013700091117     d c_CliSG_2       c                   const(0635042)
013800110923
013900110923       // -?Consegna CON Riserva specifica?
014000110923     d c_RiservaSpecif...
014100110923     d                 c                   const('RS')
014200091117
014300131008       // -?Costante per evitare l'indicazione del contratto x "S.G."?
014400091117     d c_NoContr       c                   const('NO CONTRATTO')
014500101213
014600101213       // -?Costante per controllo "caratteri solo numerici"?
014700110126     d c_Digits        c                   const('0123456789')
014800101207       // -?Numero di righe da gestire per la LdV?
014900130213     d c_Row           c                   const(36)
015000081027     d c_Row_B         c                   const(49)
015100130213     d c_Row_B2        c                   const(51)                            c_Row_B + 2
015200081030     d c_Row_Fed       c                   const(10)
015300101207       // -?Sequenza numerica?
015400080430     d c_Char          c                   const('*...+....1....+....2+
015500080430     d                                            ....+....3....+....4+
015600080430     d                                            ....+....5....+....6+
015700080430     d                                            ....+....7....+....8+
015800080430     d                                            ....+....9....+...10+
015900080430     d                                            ....+...11....+...12')
016000130227
016100130227       // -?Dati di default?
016200130227     d c_Mittente      c                   const('ced')
016300130227     d c_at            c                   const('@brt.it')
016400130227     d c_Sede          c                   const('046')
016500130227
016600130227       // -?Comandi di override al PrtF "corpo e-mail"?
016700130227     d c_CmdOvrPrtF    c                   const('OVRPRTF +
016800130227     d                                           file(PRTEMAIL) +
016900130227     d                                           pagesize(66 132) +
017000130227     d                                           lpi(6) cpi(10) +
017100130227     d                                           ovrscope(*actgrpdfn) +
017200130227     d                                           ')
017300130227     d c_CmdDltOvr     c                   const('DLTOVR +
017400130227     d                                            file(PRTEMAIL) +
017500130227     d                                            lvl(*actgrpdfn)')
017600101213
017700101213       //--------------------------------------------------------------
017800101213       //?Definizione schiere.                                         ?
017900101213       //--------------------------------------------------------------
018000101213
018100101213       // -?Giorni della settimana e relativa decodifica?
018200070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
018300070306     d $Gio            s             15    dim(9)   alt($GgS)
018400110202
018500110202       // -?Codice / Decodifica LOG x FIILV00F?
018600110202     d $CoL            s                   like(ILVcol)
018700130313     d                                     dim(10)  ctdata  perrcd(1)
018800110202     d $DeL            s                   like(ILVnot)
018900130313     d                                     dim(10)  alt($CoL)
019000101213
019100101207       // -?Dati da restituire?
019200130214     d LdV             ds           123    occurs(c_Row_B2) inz
019300080508     d   LdvPrg                       3s 0 inz
019400080430     d   LdvTxt                     120    inz
019500101213
019600101213       // -?Note e Particolarità?
019700070306     d $NP             s                   dim(6)   inz  like(Pnp1)
019800101213       // -?Consegne Precedenti?
019900070319     d $CP             s                   dim(20)  inz(*all'9')
020000070319     d                                                   like(Elem_CP)
020100070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
020200101213
020300101213       //--------------------------------------------------------------
020400101213       //?Definizione strutture dati.                                  ?
020500101213       //--------------------------------------------------------------
020600101213
020700080430     d KPJBA         e ds                  inz
020800101213
020900131008       // -?Dati LDV da restituire per l'immagine?
021000080506     d FNLSB5ds      e ds                  inz
021100080430
021200110202       // -?Records in output?
021300110202       // ·? 1ª riga?
021400081021     d ds_01         e ds                  inz  extname(FISP3001ds)
021500081021     d                                          prefix(P : 3)
021600110202       // ·? 2ª riga (vuota)?
021700110202       // ·? 3ª riga?
021800080430     d ds_03         e ds                  inz  extname(FISP3003ds)
021900080430     d                                          prefix(P : 3)
022000110202       // ·? 4ª riga?
022100080430     d ds_04         e ds                  inz  extname(FISP3004ds)
022200080430     d                                          prefix(P : 3)
022300110202       // ·? 5ª riga?
022400080430     d ds_05         e ds                  inz  extname(FISP3005ds)
022500080430     d                                          prefix(P : 3)
022600110202       // ·? 6ª riga (vuota)?
022700110202       // ·? 7ª riga?
022800080430     d ds_07         e ds                  inz  extname(FISP3007ds)
022900080430     d                                          prefix(P : 3)
023000110202       // ·? 8ª riga (vuota)?
023100110202       // ·? 9ª riga?
023200080430     d ds_09         e ds                  inz  extname(FISP3009ds)
023300080430     d                                          prefix(P : 3)
023400110202       // ·?10ª riga?
023500080430     d ds_10         e ds                  inz  extname(FISP3010ds)
023600080430     d                                          prefix(P : 3)
023700110202       // ·?11ª riga?
023800080430     d ds_11         e ds                  inz  extname(FISP3011ds)
023900080430     d                                          prefix(P : 3)
024000110202       // ·?12ª riga?
024100080430     d ds_12         e ds                  inz  extname(FISP3012ds)
024200080430     d                                          prefix(P : 3)
024300110202       // ·?13ª riga?
024400080430     d ds_13         e ds                  inz  extname(FISP3013ds)
024500080430     d                                          prefix(P : 3)
024600110202       // ·?14ª riga?
024700080430     d ds_14         e ds                  inz  extname(FISP3014ds)
024800080430     d                                          prefix(P : 3)
024900130205     d ds_140        e ds                  inz  extname(FISP30140s)
025000130205     d                                          prefix(P : 3)
025100110202       // ·?15ª riga?
025200080430     d ds_15         e ds                  inz  extname(FISP3015ds)
025300080430     d                                          prefix(P : 3)
025400110202       // ·?16ª riga?
025500080430     d ds_16         e ds                  inz  extname(FISP3016ds)
025600080430     d                                          prefix(P : 3)
025700110202       // ·?17ª riga?
025800080430     d ds_17         e ds                  inz  extname(FISP3017ds)
025900080430     d                                          prefix(P : 3)
026000110202       // ·?18ª riga?
026100080430     d ds_18         e ds                  inz  extname(FISP3018ds)
026200080430     d                                          prefix(P : 3)
026300130205     d ds_180        e ds                  inz  extname(FISP30180s)
026400130205     d                                          prefix(P : 3)
026500110202       // ·?19ª riga?
026600080430     d ds_19         e ds                  inz  extname(FISP3019ds)
026700080430     d                                          prefix(P : 3)
026800110202       // ·?20ª riga?
026900080430     d ds_20         e ds                  inz  extname(FISP3020ds)
027000080430     d                                          prefix(P : 3)
027100110202       // ·?21ª riga?
027200080430     d ds_21         e ds                  inz  extname(FISP3021ds)
027300080430     d                                          prefix(P : 3)
027400110202       // ·?22ª riga?
027500080430     d ds_22         e ds                  inz  extname(FISP3022ds)
027600080430     d                                          prefix(P : 3)
027700110202       // ·?23ª riga?
027800080430     d ds_23         e ds                  inz  extname(FISP3023ds)
027900080430     d                                          prefix(P : 3)
028000110202       // ·?24ª riga?
028100080430     d ds_24         e ds                  inz  extname(FISP3024ds)
028200080430     d                                          prefix(P : 3)
028300110202       // ·?25ª riga?
028400080430     d ds_25         e ds                  inz  extname(FISP3025ds)
028500080430     d                                          prefix(P : 3)
028600110202       // ·?26ª riga (vuota)?
028700110202       // ·?27ª riga?
028800080430     d ds_27         e ds                  inz  extname(FISP3027ds)
028900080430     d                                          prefix(P : 3)
029000110202       // ·?28ª riga?
029100080430     d ds_28         e ds                  inz  extname(FISP3028ds)
029200080430     d                                          prefix(P : 3)
029300110202       // ·?29ª riga?
029400081021     d ds_29         e ds                  inz  extname(FISP3029ds)
029500081021     d                                          prefix(P : 3)
029600110202       // ·?30ª riga?
029700081021     d ds_30         e ds                  inz  extname(FISP3030ds)
029800081021     d                                          prefix(P : 3)
029900110215       // ·?31ª riga?
030000110215     d ds_31         e ds                  inz  extname(FISP3031ds)
030100110215     d                                          prefix(P : 3)
030200110202       // ·?32ª riga?
030300081021     d ds_32         e ds                  inz  extname(FISP3032ds)
030400081021     d                                          prefix(P : 3)
030500110202       // ·?33ª riga?
030600081021     d ds_33         e ds                  inz  extname(FISP3033ds)
030700081021     d                                          prefix(P : 3)
030800110202       // ·?34ª riga?
030900081021     d ds_34         e ds                  inz  extname(FISP3034ds)
031000081021     d                                          prefix(P : 3)
031100110202       // ·?35ª riga (vuota)?
031200110202       // ·?36ª riga (vuota)?
031300110202       // ·?37ª riga e successive?
031400081023     d ds_37         e ds                  inz  extname(FISP3037ds)
031500081021     d                                          prefix(P : 3)
031600110202       // ·?Righe x FedEx?
031700081030     d ds_Fed        e ds                  inz  extname(FISP304Fds)
031800081030     d                                          prefix(P : 3)
031900130411
032000130411       // -?Flag operativi in TITAS30C?
032100130411     d dTASflo       e ds                  inz     qualified
032200120702
032300130214       // -?Dati aggiuntivi in FIILV00F?
032400120702     d dILVlibero    e ds                  inz     qualified
032500101220
032600101220       // -?Parametri per conversione importi in Euro?
032700061107     d YEURCOds      e ds                  inz
032800110202
032900101220       // -?Dati da AZORG00F?
033000070424     d OG143ccm      e ds                  inz     extname(OG143)
033100070424     d                                             prefix(§CM:3)
033200070424     d OG146arr      e ds                  inz     extname(OG146)
033300070424     d                                             prefix(A)
033400070424     d OG146cf£6     e ds                  inz     extname(OG146)
033500070424     d                                             prefix(£6:1)
033600070424     d OG148arr      e ds                  inz     extname(OG148)
033700070424     d                                             prefix(A)
033800070424     d OG148cf£6     e ds                  inz     extname(OG148)
033900070424     d                                             prefix(£6:1)
034000070424     d OG150arr      e ds                  inz     extname(OG150)
034100070424     d                                             prefix(A)
034200110202
034300101220       // -?Dati da FIAR400F?
034400061108     d dsBL4A        e ds                  inz
034500171117     d*// dsBL4I        e ds                  inz
034600071015     d dsBL4M        e ds                  inz
034700121018     d dTA4Q         e ds                  inz
034800110228
034900110228       // -?Dati da FIDST00F?
035000110228     d dDSTflr       e ds                  inz
035100110202
035200101220       // -?Tabelle?
035300131206     d ds$2          e ds
035400061107     d ds1A          e ds                  inz
035500061107     d ds1P          e ds                  inz
035600070306     d ds2A          e ds                  inz
035700061107     d ds3A          e ds                  inz
035800061107     d ds5E          e ds                  inz
035900061107     d dsCC          e ds
036000061107     d dsCV          e ds                  inz
036100061107     d dsEI          e ds                  inz
036200061107     d dsTB          e ds                  inz
036300061107      *
036400030515     d TIBS02ds      e ds                  inz
036500040607     d   T02mod      e                     inz('C')
036600030805     d dGED          e ds
036700130206     d dILV          e ds
036800061031     d dVPOdeco      e ds                  inz
036900031124     d dAR5gen       e ds                  inz
037000061030     d dAR5bnb       e ds                  inz
037100170517     d dCLI          e ds                  inz
037200080724
037300131210       // -?Flag operativi di FNEVB - campo EVBNOT?
037400131206     d dEVB01        e ds                  inz     qualified
037500101220       // -?Flag operativi di FNORM - campo ORMFLO?
037600100517     d dORM01        e ds                  inz
037700101220       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
037800030515     d TRUL21ds      e ds                  inz
037900120702     d   I21tla      e                     inz('L')
038000101220       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
038100070212     d FNLV55ds      e ds                  inz
038200120702     d   D55tpt      e                     inz('6')
038300101220       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
038400051129     d FNLV80ds      e ds                  inz
038500120702     d   ILV80tla    e                     inz('C')
038600101220       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
038700090619     d TNSD99ds      e ds                  inz
038800120702     d   D98tla      e                     inz('C')
038900130227
039000130227       // -?Tab. "MRA" = Bart-Mailing?
039100130227     d dMRAdan       e ds                  inz
039200130227     d   §MRAddes    e                     inz('PDA-Oltre 100 log "01"+
039300130227     d                                             ')
039400130227     d   §MRAdreg    e                     inz('COR')
039500130227     d   §MRAdmitt   e                     inz(c_Mittente)
039600130227     d   §MRAddest   e                     inz('stefano.merighi+
039700130227     d                                          @brt.it;')
039800130227     d   §MRAdidpro  e                     inz('1')
039900130227     d   §MRAdoutqi  e                     inz('EMAILIN')
040000130227
040100130227       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
040200130227     d TRTCM1ds      e ds                  inz
040300130716         //?·§CM1mitt = Indirizzo e-mail del mittente?
040400130227     d   §CM1mitt    e                     inz(c_Mittente)
040500130716         //?·§CM1dst  = Indirizzo e-mail del destinatario?
040600130227     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
040700130716         //?·§CM1tips = Tipo lettera via e-mail:?
040800130716         // ?"LET" = testo allegato in corpo con logo?
040900130716         //         ?(richiede righe libere iniziali per il logo)?
041000130716         // ?"COR" = testo integrato senza logo?
041100130716         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
041200130716         // ?"SP1" = spool allegato (in formato PDF)?
041300130227     d   §CM1tips    e                     inz('COR')
041400130716         //?·§CM1po   = Sede?
041500130227     d   §CM1po      e                     inz(c_Sede)
041600130716         //?·§CM1var  = Oggetto e-mail?
041700130227     d   §CM1var     e                     inz('*OBJM*+
041800130227     d                                     Sped. elaborata oltre 100 +
041900130227     d                                     volte in FIILV - vedi log +
042000130227     d                                     "01"')
042100130716         //?·§CM1sts  = Stato?
042200130227     d   §CM1sts     e                     inz(*off)
042300130716         //?·§CM1idp  = Id processo?
042400130227     d   §CM1idp     e                     inz('2')
042500101220
042600101220       // -?Status ds?
042700030515     d Status         sds
042800120702     d   SDSpgm          *proc
042900120702     d   SDSprm          *parms
043000120702     d   SDSusr              254    263
043100101220
043200101220       // -?Parametro Numero Distinta?
043300101220     d ds_Distinta     ds             9    inz  qualified
043400101220     d   P_Fgs                        3s 0 inz
043500101220     d   P_Ndc                        6s 0 inz
043600080430
043700101207       // -?Parametro Numero Spedizione?
043800110207     d ds_Spedizione   ds            16    inz  qualified
043900080430     d   P_Aas                        4s 0 inz
044000080430     d   P_Lnp                        3s 0 inz
044100080430     d   P_Nrs                        2s 0 inz
044200080430     d   P_Nsp                        7s 0 inz
044300101220
044400080519     d BARcodeDS       ds            17    inz
044500120702     d   BARcdeAAS                    2s 0 inz
044600120702     d   BARcdeLNP                    3s 0 inz
044700120702     d   BARcdeNRS                    2s 0 inz
044800120702     d   BARcdeNSP                    7s 0 inz
044900101220
045000030515     d KEYpar          ds             8    inz
045100120702     d   WTPpar                1      1    inz
045200120702     d   WCDpar                2      3    inz
045300120702     d   Wprogr                4      8    inz('S0001')
045400101220
045500140303     d Elem_CP         ds                  inz
045600120702     d   WCPdcm                       8s 0 inz
045700140303     d   WCPocm                       4s 0 inz
045800120702     d   WCPncl                       5s 0 inz
045900140303     d   WCPfir                            inz  like(EVB2dati)
046000101213
046100101213       //--------------------------------------------------------------
046200101213       //?Definizione variabili globali.                               ?
046300101213       //--------------------------------------------------------------
046400101213
046500101220       // -?Parametri?
046600080430     d Spedizione      s             16
046700101217     d Distinta        s              9
046800101217     d DataOraCrea     s             14
046900080430     d Risultato       s              2
047000080715     d RootFolder      s            100
047100080519     d FileName        s             30
047200080731     d TemplateName    s             20
047300110201     d LastRow         s            120
047400131016     d RtnEsito        s             10i 0                                      // -?NON usato?
047500131016     d RtnOpCode       s             10                                         // -?NON usato?
047600131016     d RtnStatus       s             10i 0                                      // =?SQLcode?
047700080430     d P_Prt           s              1
047800101220
047900130214       // -?Flags booleani?
048000101220     d $Mamma          s               n   inz
048100130215     d $Mamma_FedEx    s               n   inz
048200101220     d $FIAR8          s               n   inz
048300130411     d $§PDF           s               n   inz
048400110204     d $FIILV          s               n   inz
048500101220     d $NoDesPartic    s               n   inz
048600110117     d*// $BolloVirt      s               n   inz(*off)
048700120113     d $No_x_Cons      s               n   inz
048800101220     d $ConsParz       s               n   inz
048900101220     d $CAeur          s               n   inz
049000100517     d $PreavvORM      s               n   inz
049100101220     d $GeoAtt         s               n   inz
049200131206     d $1o_Passaggio   s               n   inz
049300101220
049400101220       // -?Indici di schiera?
049500080523     d XX              s              3  0 inz
049600140304     d YY              s              3  0 inz
049700080523     d WX              s              3  0 inz
049800130313
049900130313       // -?Nome esteso Libreria/File del file TITAS30C?
050000130313     d wLibFileTITAS   s             21a   inz
050100080724
050200101207       // -?Campi di stampa?
050300130227     d P_Fgs2          s                   inz  like(ds_Distinta.P_Fgs)
050400130227     d P_Ndc2          s                   inz  like(ds_Distinta.P_Ndc)
050500101220
050600101220       // -?Campi di comodo alfanumerici?
050700030717     d w_CoOr          s             35    inz
050800081030     d wAWB            s             12    inz
050900030515     d w001a           s              1    inz
051000030515     d w008a           s              8    inz
051100030515     d w015a           s             15    inz
051200100517     d w053a           s             53    inz
051300070424     d wDateA          s              8    inz
051400070717     d wGeoTel         s              1    inz
051500070717     d wGeoCon         s              1    inz
051600080718     d wSysName        s              8    inz
051700130411      *? x TEST ?
051800130411     d $Test           s              3    inz
051900101220
052000101220       // -?Campi di comodo numerici?
052100030515     d w0080           s              8  0 inz
052200030624     d w0112           s             11  2 inz
052300030515     d w0140           s             14  0 inz
052400071105     d wDate           s              8  0 inz
052500090120     d w_Row           s              3  0 inz(c_Row)
052600110309     d wCountRec01     s              5  0 inz
052700101220
052800131008       // -?Campi utilizzati per impostazione dell'output?
052900110117     d*// wBolloVirt      s              4  2 inz
053000030515     d Wcas            s             13  3 inz
053100030515     d Wiva            s             11  2 inz
053200071015     d Wpdc4A          s              4    inz
053300030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
053400090619     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
053500101220
053600101220       // -?Campi utilizzati per il salvataggio momentaneo di dati?
053700040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
053800121018     d wB0cfi          s                   like(DB0cpi)   inz
053900121018     d wB0piv          s                   like(DB0cpi)   inz
054000120702     d wB0nrs          s                   like(ARBnrs)   inz
054100070514     d wB0ntc          s                   like(ARBntc)   inz
054200080523     d wB0dcm          s                   like(ARBdcm)   inz
054300070514     d wB0dcp          s                   like(ARBdcp)   inz
054400081120     d wB0tmc          s                   like(ARBtmc)   inz
054500130205     d wB0fiv          s                   like(AR6fiv)   inz
054600080506     d Wnsc            s                   like(ARTnsc)   inz
054700080506     d Wtrc            s                   like(AR4trc)   inz
054800030515     d Wctr            s                   like(DB0ctr)   inz
054900031124     d Wdsp            s                   like(DB0dft)   inz
055000090619     d WddtCLN         s                   like(DB0drt)   inz
055100090619     d WdspCLN         s              6  0                inz
055200030515     d Wsvx            s                   like(AR6sv1)   inz
055300030515     d Wvax            s                   like(AR6va1)   inz
055400110203     d wCod_Log        s                   like(ILVcol)   inz
055500090619
055600140303       //--------------------------------------------------------------
055700140303       //?Definizione procedure usate.                                 ?
055800090619       //--------------------------------------------------------------
055900100322
056000100322       // -?Procedure name: getCurrentSystemName?
056100100322       //  ?Purpose:        Reperisce l'attributo di rete current System Name?
056200100322       //  ?Returns:        Current System Name?
056300130716       //  ?(vedi "QWCRNETA")?
056400100322     d getCurrentSystemName...
056500100322     d                 pr             8a
056600100322
056700100322       //--------------------------------------------------------------
056800140303       //?Definizione procedure esterne.                               ?
056900100322       //--------------------------------------------------------------
057000100322
057100110201       // -?Calcolo check digit del barcode?
057200110201     d TRUL28ds      e ds                  inz
057300110201     d  I28tla       e                     inz('L')
057400110201     d  I28mod       e                     inz('BAR')
057500110201     d TrUl28r1        pr                  extpgm('TRUL28R1')
057600110201     d  trul28ds                           likeds(TRUL28ds)
057700110201
057800100322       // -?Calcolo tempi di delivery?
057900100322      /copy gaitrasrc/srcProtoPR,TNSD99R
058000130206
058100130206       // -?Ricerca/Controllo tabelle?
058200130206      /copy gaitrasrc/srcProtoPR,TIBS02R
058300100322
058400110201       // -?Parametri per API "QWCRNETA"?
058500110201      /copy QSYSINC/QRPGLESRC,QWCRNETA
058600110201     d qWcrNetA_dataReturned...
058700110201     d                 ds                  qualified inz
058800110201     d  qWcRDR                             likeds(qWCRDR)
058900110201     d  qWcOAT                       10i 0 dim(00001)
059000110201     d  qWcARRAY                     24
059100110201     d qWCRNETA_networkAttributeInformationTable...
059200110201     d                 ds                  qualified inz
059300110201     d  qWcRAT                             likeds(qWCRAT)
059400110201     d  qWcDATA                       8
059500100322       // -?Reperimento attributo di rete current System Name?
059600100322       //  ?(vedi "getCurrentSystemName")?
059700100322      /copy gaitrasrc/srcProtoPR,QWCRNETA
059800130227
059900130227       // -?Parametri API QCAPCMD (Process Commands)?
060000130227     d Qcmd            s           2048    inz  varying
060100130227      /copy qSysInc/qRpgleSrc,QCAPCMD
060200130227       // -?API QCAPCMD (Process Commands)?
060300130227      /copy gaitrasrc/srcProtoPR,QCAPCMD
060400130227
060500130227       // -?Parametri gestione errori API.?
060600130227      /copy qsysinc/qRpgleSrc,QUSEC
060700080710
060800080710       //--------------------------------------------------------------
060900140303       //?Definizione key-list.                                        ?
061000080710       //--------------------------------------------------------------
061100130313
061200130422       // -?File FIIRD11L?
061300130422     d k01fiird11    e ds                  extname(FIIRD11L : *key)
061400130313     d                                     prefix(k_)
061500130313     d                                     inz
061600130313
061700130313       // -?File TITAS30C?
061800130313     d k05titas30    e ds                  extname(TITAS30C : *key)
061900130313     d                                     prefix(k_)
062000130313     d                                     inz
062100131206
062200131206       // -?File FNEVB04L?
062300131206     d keyFNEVB04    e ds                  extname(FNEVB04L : *key)
062400131206     d                                     prefix(k_)
062500131206     d                                     inz
062600140303
062700140303       // -?File FNEVB22L?
062800140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
062900140303     d                                     prefix(k_)
063000140303     d                                     inz
063100170517
063200170517       // -?File TFNTC01L?
063300170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
063400170517     d                                     prefix(k_)
063500170517     d                                     inz
063600080710
063700101207       // -?File FIDST01L?
063800110228     d k03fidst01    e ds                  extname(FIDST01L : *key)
063900080710     d                                     prefix(k_)
064000080710     d                                     inz
064100090216
064200110202       // -?File FIILV01L?
064300110202     d k06fiilv01    e ds                  extname(FIILV01L : *key)
064400110207     d                                     prefix(k_)
064500090216     d                                     inz
064600030515
064700101220       //==============================================================
064800101220       //?RIEPILOGO INDICATORI                                         ?
064900101220       //--------------------------------------------------------------
065000101220       // 31    - di comodo
065100101220       // 35    - Porto Franco: segnala
065200101220       //         "PER RICEVUTA DELLA MERCE SENZA RISERVE"
065300101220       //?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia
065400101220       //         e indica "TOTALE DA ESIGERE AL RITIRO"
065500120524       // 70    - Trattasi di BOLLA BRT
065600101220       // 71    - Trattasi di BOLLA DPD
065700101220       // 72    - Trattasi di BOLLA FedEx
065800101220       // 73    - Trattasi di BOLLA EuroExpress
065900101220       //==============================================================
066000030515
066100101213       //--------------------------------------------------------------
066200101213       //?M A I N - L I N E                                            ?
066300101213       //--------------------------------------------------------------
066400101213
066500030515     c     *Entry        plist
066600080430     c                   parm                    Spedizione
066700101217     c                   parm                    Distinta
066800101217     c                   parm                    DataOraCrea
066900080430     c                   parm                    Risultato
067000080715     c                   parm                    RootFolder
067100080519     c                   parm                    FileName
067200080731     c                   parm                    TemplateName
067300110201     c                   parm                    LastRow
067400080430     c                   parm                    RtnEsito
067500080430     c                   parm                    RtnOpCode
067600080430     c                   parm                    RtnStatus
067700080430     c                   parm                    P_Prt
067800080430      *
067900110519      *?P_Prt:?non passato se richiamato da server, serve solo se?
068000110519      *       ?chiamato da menù AS400 x test?
068100101221      *   Valori: qualsiasi = richiamato da pgm. per test (stampa)
068200080430      *
068300070123      *
068400130924      * Operazioni iniziali
068500131016       //?SE rilevati errori nei parametri di input =>?
068600131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
068700070123     c                   exsr      sr_Start
068800030910      *
068900130924      * Pulizia dei campi di output e/o di lavoro
069000130924     c                   exsr      sr_ClrFLD
069100030515      *
069200130924      * Caricamento dei dati per l'immagine della LdV
069300131016       //?SE rileva errori nella distinta o nella spedizione =>?
069400131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
069500110208     c                   exsr      sr_CarARB
069600030515      *
069700130924      * Controllo del tipo LDV di cui generare l'immagine
069800110208     c                   exsr      sr_TipoLDV
069900030515      *
070000080430      * Impostazione dei campi di output con quelli della DS
070100110208     c                   exsr      sr_PrepPRT
070200030515      *
070300080430      * Decodifica dei dati della LDV
070400110208     c                   exsr      sr_DecodLDV
070500080806      *
070600080806      * Impostazione dati per immagine LdV
070700131016     c                   exsr      sr_DatiOUT
070800030515      *
070900030515      * Chiusura pgm
071000080430     c                   exsr      sr_End
071100030515      *
071200030515      *---------------------------------------------------------------*
071300050217      *?OPERAZIONI INIZIALI                                          ?*
071400030515      *---------------------------------------------------------------*
071500030515     c     *InzSR        BEGSR
071600070424      *
071700070424      * Reperisco la data del giorno
071800070424     c                   time                    w0140
071900070424     c                   move      w0140         w0080
072000070424     c     *eur          movel     w0080         Data_EUR
072100070424     c     *iso          movel     Data_EUR      w0080
072200070424     c                   move      w0080         wDateA
072300071105     c                   eval      wDate  = w0080
072400030515      *
072500030515      * Imposto i campi chiave "fissi"
072600040223     c                   eval      TBLkut = 1
072700100901     c                   eval      ARStrc = 'C'
072800030515      *
072900040617      * Impostazione dati in KPJBA
073000070308     c                   eval      KNMUS  = SDSusr
073100030515      *
073200030515      * Reperisco la moneta di conto
073300030515     c                   clear                   dGED
073400040607     c                   clear                   TIBS02ds
073500040607     c                   eval      T02mod = 'C'
073600040607     c                   eval      T02cod = 'GED'
073700040607     c                   eval      T02ke1 = '1         '
073800030515     c                   call      'TIBS02R'
073900030515     c                   parm                    KPJBA
074000040607     c                   parm                    TIBS02ds
074100030515if  1c                   if        T02err = *blanks
074200030515     c                   movel     T02uni        dGED
074300030515e   1c                   endif
074400061031      *
074500131008      * Reperisco data di decorrenza indicaz. "Per ricevuta di ...
074600131008      *                                        senza riserve"
074700131008      *              e di decorrenza indicaz. "Consegna al mattino"
074800061031     c                   clear                   dVPOdeco
074900061031     c                   clear                   TIBS02ds
075000061031     c                   eval      T02mod = 'C'
075100071112     c                   eval      T02cod = 'VPO'
075200061031     c                   eval      T02ke1 = 'DECO      '
075300061031     c                   call      'TIBS02R'
075400061031     c                   parm                    KPJBA
075500061031     c                   parm                    TIBS02ds
075600061031if  1c                   if        T02err = *blanks
075700061031     c                   movel     T02uni        dVPOdeco
075800061031e   1c                   endif
075900030515      *
076000030515     c                   ENDSR
076100070123      *
076200101220      /free
076300101220
076400101220       //--------------------------------------------------------------
076500101220       //?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?
076600101220       //--------------------------------------------------------------
076700101220       BEGSR  sr_Start;
076800101220
076900101220         exec sql  set option  dynusrprf = *owner,
077000101220                               closqlcsr = *endmod;
077100101220
077200101221         *inLR = *on;
077300080509
077400110517         // -?Impostazione iniziale dei dati di ritorno?
077500080509         Risultato = 'OK';
077600110202         clear  RootFolder;
077700110202         clear  FileName;
077800120524         //TemplateName = 'ldvbase00 ';   //?Standard - BARTOLINI senza box aggiuntivo?
077900120524         //TemplateName = 'ldvbase10 ';   //?Standard - BARTOLINI-BRT senza box aggiuntivo?
078000130924         TemplateName = 'ldvbase20 ';     //?Standard - BRT senza box aggiuntivo?
078100110202         clear  LastRow;
078200110202         clear  RtnEsito;
078300110202         clear  RtnOpCode;
078400110202         clear  RtnStatus;
078500110207
078600110207         // -?Apertura archivio di output - comunque utilizzato?
078700110207         if  NOT  %open(FIILV01L);
078800110207           open(e)  FIILV01L;
078900110207           if  %error;
079000110207             exsr  sr_ErrOpenFile;
079100110207           endif;
079200110207         endif;
079300110207
079400110202         clear  k06fiilv01;
079500110203         clear  FIILV000;
079600080509
079700110203         // »? STEP 0 ?«
079800131016         // -?Controllo parametri ricevuti.  SE rileva errori =>?
079900131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
080000130716         exsr  sr_STEP_0_CtrlParms;
080100110203
080200080430
080300101213         // -?Impostazione spedizione ricevuta come parametro?
080400080430         ds_Spedizione = Spedizione;
080500110126         // -?Impostazione distinta ricevuta come parametro?
080600110126         ds_Distinta = Distinta;
080700080430
080800101213         // -?Impostazione iniziale dei dati nella ds FNLSB5DS?
080900130924         //clear FNLSB5ds;            ?(già così)?
081000080430         DB0tbo = 'A';
081100110202         DB0aas = ds_Spedizione.P_Aas;
081200110202         DB0lnp = ds_Spedizione.P_Lnp;
081300110202         DB0nrs = ds_Spedizione.P_Nrs;
081400110202         DB0nsp = ds_Spedizione.P_Nsp;
081500080519
081600101213         // -?Impostazione campo di output "identificativo spedizione"?
081700080519         clear  BarCodeDS;
081800080519         BARcdeAAS = %int( %subst( %editc( DB0aas : 'X' ) : 3 : 2) );
081900080519         BARcdeLNP = DB0lnp;
082000080519         BARcdeNRS = DB0nrs;
082100080519         BARcdeNSP = DB0nsp;
082200080519         FileName  = BarCodeDS;
082300130313
082400130716         // -?Impostazione chiave di accesso al file FIIRD11L?
082500130313         k_IRDfgsndc = Distinta;
082600130313
082700130313         // -?Impostazione chiave di accesso al file TITAS30C?
082800130313         clear  k05titas30;
082900130313         k_TASaas = DB0aas;
083000130313         k_TASlnp = DB0lnp;
083100130313         k_TASnrs = DB0nrs;
083200130313         k_TASnsp = DB0nsp;
083300131206
083400131206         // -?Impostazione chiave di accesso al file FNEVB04L?
083500131206         clear  keyFNEVB04;
083600131206         k_EVBaas = DB0aas;
083700131206         k_EVBlnp = DB0lnp;
083800131206         k_EVBnrs = DB0nrs;
083900131206         k_EVBnsp = DB0nsp;
084000131210         //k_EVBcev = 'MIC';          ?(leggere TUTTI i rec.)?
084100140303
084200140303         // -?Impostazione chiave di accesso al file FNEVB22L?
084300140303         clear  keyFNEVB22;
084400140303         k_EVB2aas = DB0aas;
084500140303         k_EVB2lnp = DB0lnp;
084600140303         k_EVB2nrs = DB0nrs;
084700140303         k_EVB2nsp = DB0nsp;
084800140303         //k_EVB2dtv = .....;
084900140303         //k_EVB2orv = .....;
085000140303         //k_EVB2cev = 'P  ';
085100110204
085200110204         // -?Impostazione chiave di accesso al file FIILV01L?
085300110204         k_ILVaas = DB0aas;
085400110204         k_ILVlnp = DB0lnp;
085500110204         k_ILVnrs = DB0nrs;
085600110204         k_ILVnsp = DB0nsp;
085700110225         k_ILVfgs = ds_Distinta.p_Fgs;
085800110225         k_ILVndc = ds_Distinta.p_Ndc;
085900120702         clear  dILVlibero;
086000130411
086100130411
086200130411         IF  '1' <> '1';
086300130411
086400130924           // -? CHIODI MOMENTANEI PER TEST                                 ?
086500130924           //  ? da attivare per ricavare immagini di esempio forzando sped ?
086600130924           clear  $Test;
086700130411           Select;
086800130411             // -?Spedizione FedEx?
086900130411             When  Spedizione = '2011050001843581' and Distinta = '145040335';
087000130411               $Test = 'FED';
087100130411             // -?Spedizione DPD?
087200130411             When  Spedizione = '2011190000579210' and Distinta = '145040339';
087300130411               $Test = 'DPD';
087400130411             // -?Spedizione con Consegna Parziale Precedente?
087500130411             When  Spedizione = '2011001000816011' and Distinta = '145040361';
087600130411               $Test = 'C_P';
087700130411             // -?Spedizione Normale?
087800130411             When  Spedizione = '2011163000042281' and Distinta = '145040333';
087900130411               $Test = 'NOR';
088000130411             // -?Altra spedizione: NON ELABORABILE?
088100130411             Other;
088200130411               Risultato = 'NO';
088300130924               //RtnOpCode = 'NO_IN_TEST';
088400130924               //RtnEsito  = -1;
088500130924               // -?Uscita dalla s.p.?
088600130411               exsr  sr_End;
088700130411           EndSl;
088800130411
088900130411         ENDIF;
089000130411
089100080430
089200110208         // -?Apertura archivi di input - comunque utilizzati?
089300110207         if  NOT  %open(TABEL00F);
089400110207           open(e)  TABEL00F;
089500110207           if  %error;
089600110207             exsr  sr_ErrOpenFile;
089700110207           endif;
089800110207         endif;
089900110207
090000110207         if  NOT  %open(AZORG01L);
090100110207           open(e)  AZORG01L;
090200110207           if  %error;
090300110207             exsr  sr_ErrOpenFile;
090400110207           endif;
090500110207         endif;
090600110207
090700110207         if  NOT  %open(FNARB01L);
090800110207           open(e)  FNARB01L;
090900110207           if  %error;
091000110207             exsr  sr_ErrOpenFile;
091100110207           endif;
091200110207         endif;
091300110207         if  NOT  %open(FIAR601L);
091400110207           open(e)  FIAR601L;
091500110207           if  %error;
091600110207             exsr  sr_ErrOpenFile;
091700110207           endif;
091800110207         endif;
091900110207         if  NOT  %open(FIAR701L);
092000110207           open(e)  FIAR701L;
092100110207           if  %error;
092200110207             exsr  sr_ErrOpenFile;
092300110207           endif;
092400110207         endif;
092500110207         if  NOT  %open(FIAR901L);
092600110207           open(e)  FIAR901L;
092700110207           if  %error;
092800110207             exsr  sr_ErrOpenFile;
092900110207           endif;
093000110207         endif;
093100110207         if  NOT  %open(FIDST01L);
093200110207           open(e)  FIDST01L;
093300110207           if  %error;
093400110207             exsr  sr_ErrOpenFile;
093500110207           endif;
093600110207         endif;
093700101213
093800101213         if  NOT  %open(FIAR401L);
093900101213           open(e)  FIAR401L;
094000101213           if  %error;
094100101213             exsr  sr_ErrOpenFile;
094200101213           endif;
094300101213         endif;
094400101213         if  NOT  %open(FIAR501L);
094500101213           open(e)  FIAR501L;
094600101213           if  %error;
094700101213             exsr  sr_ErrOpenFile;
094800130411           endif;
094900130411         endif;
095000101213
095100101220       ENDSR;
095200101220
095300101213       //--------------------------------------------------------------
095400101213       //?Restituzione errore in fase di apertura di un file           ?
095500101213       //--------------------------------------------------------------
095600101213       BEGSR  sr_ErrOpenFile;
095700101213
095800101213         Risultato = 'NO';
095900130924         //RtnOpCode = 'ERR_OPEN  ';
096000101213         //RtnEsito  = -1;
096100101213
096200130924         // -?Uscita dalla s.p.?
096300101213         exsr  sr_End;
096400101213
096500101213       ENDSR;
096600130716
096700130716       //--------------------------------------------------------------
096800130716       //?»  STEP 0  «                                                 ?
096900130716       //?Controllo dei parametri ricevuti, reperimento nome del       ?
097000130716       //?sistema e dei percorsi per le immagini in base ad esso.      ?
097100130716       //--------------------------------------------------------------
097200130716       BEGSR  sr_STEP_0_CtrlParms;
097300130716
097400130716         //   _________________________________________________________
097500130716         // -? Controllo lunghezza e numericità dei parametri ricevuti ?
097600130716         //   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
097700130716         SELECT;
097800130716
097900130716           // -?NUMERO SPEDIZIONE?
098000130716           WHEN  ( %len( %trimr( Spedizione ) ) )  <> 16     or
098100130716                 ( %check (c_Digits : Spedizione) ) > *zero  or
098200130716                                      Spedizione   <= *zero;
098300130716             Risultato = 'XX';
098400130924             //RtnOpCode = 'ERR_NUMSPE';
098500130716             //RtnEsito  = -1;
098600130716             // -?Registrazione log in FIILV?
098700130716             wCod_Log = '21';
098800130716             exsr  sr_WrtFIILV;
098900130716             // -?Uscita dalla s.p.?
099000130716             exsr  sr_End;
099100130716
099200130716           // -?NUMERO DISTINTA?
099300130716           WHEN  ( %len( %trimr( Distinta ) ) )  <> 9      or
099400130716                 ( %check (c_Digits : Distinta) ) > *zero  or
099500130716                                      Distinta   <= *zero;
099600130716             Risultato = 'XX';
099700130924             //RtnOpCode = 'ERR_DISTIN';
099800130716             //RtnEsito  = -1;
099900130716             // -?Registrazione log in FIILV?
100000130716             wCod_Log = '21';
100100130716             exsr  sr_WrtFIILV;
100200130716             // -?Uscita dalla s.p.?
100300130716             exsr  sr_End;
100400130716         ENDSL;
100500130716
100600130716         // ________________________________________________________
100700130716         //? Reperimento nome del sistema per impostazione percorso ?
100800130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
100900130716         wSysName = getCurrentSystemName();
101000130716         if  wSysName = *blank;
101100130716           Risultato  = 'NO';
101200130924           //RtnOpCode  = 'SYS_ERR   ';
101300130924           //RtnEsito   = -1;
101400130924           // -?Uscita dalla s.p.?
101500130716           exsr  sr_End;
101600130716         endif;
101700130716
101800130716         // ______________________________________________________
101900130716         //? Reperimento dei percorsi in base al nome del sistema ?
102000130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
102100130716         clear  dILV;
102200130716         reset  TIBS02ds;
102300170707         T02cod = 'ILG';
102400130716         T02ke1 = %subst( wSysName : 1 : 6 );
102500130716         T02ke2 = 'LINEA';
102600130716         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
102700130716         if  T02err = *blank;
102800130716           dILV = T02uni;
102900130716         endif;
103000130716
103100130716       ENDSR;
103200110208
103300110208       //--------------------------------------------------------------
103400130924       //?Pulizia dei campi di output e/o di lavoro                    ?
103500110208       //--------------------------------------------------------------
103600130924       BEGSR  sr_ClrFLD;
103700080430
103800081021         clear ds_01;
103900101213       //clear ds_02;                      ?(riga vuota)?
104000080430         clear ds_03;
104100080430         clear ds_04;
104200080430         clear ds_05;
104300101213       //clear ds_06;                      ?(riga vuota)?
104400080430         clear ds_07;
104500101213       //clear ds_08;                      ?(riga vuota)?
104600080430         clear ds_09;
104700080430         clear ds_10;
104800080430         clear ds_11;
104900080430         clear ds_12;
105000080430         clear ds_13;
105100080430         clear ds_14;
105200130205         clear ds_140;
105300080430         clear ds_15;
105400080430         clear ds_16;
105500080430         clear ds_17;
105600080430         clear ds_18;
105700130205         clear ds_180;
105800080430         clear ds_19;
105900080430         clear ds_20;
106000080430         clear ds_21;
106100080430         clear ds_22;
106200080430         clear ds_23;
106300080430         clear ds_24;
106400080430         clear ds_25;
106500101213       //clear ds_26;                      ?(vuoto)?
106600080430         clear ds_27;
106700080430         clear ds_28;
106800081021         clear ds_29;
106900081021         clear ds_30;
107000110215         clear ds_31;
107100081021         clear ds_32;
107200081021         clear ds_33;
107300081021         clear ds_34;
107400081023         clear ds_37;
107500081030         clear ds_Fed;
107600080430
107700080430      /end-free
107800030515      *
107900060524     c                   clear                   dsBL4A
108000171117     c*//                clear                   dsBL4I
108100110204     c                   clear                   dsBL4M
108200100517     c                   clear                   dOrm01
108300041207      *
108400070514     c                   reset                   $Mamma
108500130215     c                   reset                   $Mamma_FedEx
108600060921     c                   reset                   $FIAR8
108700130411     c                   reset                   $§PDF
108800110204     c                   reset                   $FIILV
108900030710     c                   reset                   $NoDesPartic
109000110117     c*//                reset                   $BolloVirt
109100040707     c                   reset                   $No_x_Cons
109200070319     c                   reset                   $ConsParz
109300030515     c                   reset                   $CAeur
109400100901     c                   reset                   $PreavvORM
109500101220     c                   reset                   $GeoAtt
109600070717     c                   clear                   wGeoTel
109700070717     c                   clear                   wGeoCon
109800070717     c                   clear                   Wdsp
109900090619     c                   clear                   WddtCLN
110000090619     c                   clear                   WdspCLN
110100030717     c                   clear                   W_CoOr
110200110117     c*//                clear                   wBolloVirt
110300030515     c                   clear                   Wcas
110400030515     c                   clear                   Wiva
110500071015     c                   clear                   Wpdc4a
110600121018     c                   clear                   wB0cfi
110700121018     c                   clear                   wB0piv
110800031015     c                   clear                   wB0nrs
110900070514     c                   clear                   wB0ntc
111000080523     c                   clear                   wB0dcm
111100070514     c                   clear                   wB0dcp
111200081120     c                   clear                   wB0tmc
111300130205     c                   clear                   wB0fiv
111400030515      *
111500030515     c                   clear                   $NP
111600070319     c                   reset                   $CP
111700070319     c                   reset                   $CPdcm
111800030515      *
111900030515     c                   movea     *zeros        *in(01)
112000030515      *
112100030515     c                   ENDSR
112200030515      *
112300030515      *---------------------------------------------------------------*
112400131008      *?Determino il modulo LDV da usare per l'immagine              ?*
112500131008      *?(se DPD, FedEx o BRT - con consegne parziali precedenti o    ?*
112600131008      *? promrmoria per l'autotrasportatore).                        ?*
112700030515      *---------------------------------------------------------------*
112800110208     c     sr_TipoLDV    BEGSR
112900070215      *
113000070212      * Impostazione della data spedizione
113100110208      * (se non già impostata in subr. "sr_CarARB")
113200070212if  1c                   if        Wdsp   =  *zeros
113300070212if  2c                   if        DB0mgs <> *zeros
113400070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
113500070212x   2c                   else
113600070212      *   prendo la data del giorno se non c'è la data spedizione
113700070212     c                   time                    w0140
113800070212     c                   move      w0140         w0080
113900070212     c     *eur          movel     w0080         Data_EUR
114000070212     c     *iso          movel     Data_EUR      Wdsp
114100070212e   2c                   endif
114200070212e   1c                   endif
114300040607      *
114400070308      * Dati della filiale del mittente
114500040607     c                   eval      WX     =  DB0ccm / 10000
114600040607if  1c                   if        WX     <> ORGfil
114700040607     c     WX            chain     AZORG
114800040607e   1c                   endif
114900040607if  1c                   if        %found(AZORG01L)
115000040607     c                   movel     ORGde3        OG143ccm
115100040607x   1c                   else
115200040607     c                   clear                   OG143ccm
115300040607e   1c                   endif
115400040311      *
115500040311      * Dati della linea di arrivo
115600131203      *?(SONO GIÀ STATI REPERITI NELLA SUBR. "SR_DECLNA",?
115700131203      *? ESEGUITA DALLA SUBR. "SR_CARARB")?
115800070212      *
115900070212      * Dati della capofila della linea di arrivo
116000131205      * - per verificare TELLUS
116100070717      *
116200070717      *** Imposta flag attivazione GEO x LNA
116300070717     c                   eval      wGeoCon = A§OGtlls
116400070717     c                   eval      wGeoTel = A§OGgeot
116500070717if  1c                   if             A§OGdda  > *zeros
116600070717     c                             and  A§OGdda  < C_DateMax_A
116700070717     c                             and  A§OGdda <= wDateA
116800101220     c                   eval      $GeoAtt = *on
116900070717e   1c                   endif
117000070717      *
117100110203if  1c                   IF             wGeoCon <> 'S'
117200070717     c                              or  wGeoTel <> 'S'
117300110203     c                              or  $GeoAtt <> *on
117400070212      * - Reperimento capofila (£6)
117500070212if  2c                   if        DB0lna <> D55lin
117600070212     c                   reset                   FNLV55ds
117700130924     c***?già così:?     eval      D55tpt =  '6'
117800070212     c                   eval      D55lin =  DB0lna
117900070212     c                   eval      D55drf =  Wdsp
118000070212     c                   call      'FNLV55R'
118100070212     c                   parm                    FNLV55ds
118200070212if  3c                   if        D55err <> *blanks
118300070212     c                   eval      D55tfa =  DB0lna
118400070212e   3c                   endif
118500070212e   2c                   endif
118600070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
118700070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
118800070212if  2c                   if        D55tfa <> ORGfil
118900070212     c     D55tfa        chain     AZORG
119000071109if  3c                   if        %found(AZORG01L)
119100070717     c                   movel     ORGde6        OG146cf£6
119200070717     c                   movel     ORGde8        OG148cf£6
119300071109x   3c                   else
119400071109     c                   clear                   OG146cf£6
119500071109     c                   clear                   OG148cf£6
119600071109e   3c                   endif
119700071109e   2c                   endif
119800070717      *** Imposta flag attivazione GEO x capofila £6
119900070717     c                   eval      wGeoCon = £6OGtlls
120000070717     c                   eval      wGeoTel = £6OGgeot
120100071109if  2c                   if             £6OGdda  > *zeros
120200070717     c                             and  £6OGdda  < C_DateMax_A
120300070717     c                             and  £6OGdda <= wDateA
120400101220     c                   eval      $GeoAtt = *on
120500071109e   2c                   endif
120600070717      *
120700070212e   1c                   ENDIF
120800040607      *
120900130411      * Reperimento dati da FIAR5 / "GEN"
121000130411      *   (per verificare se esistono Packing-List e/o PDF allegati
121100130411      *    - oltre che per reperire Nominativo e Telefono)
121200130411     c                   exsr      RepAR5gen
121300130411      *
121400130411      * Verifica se LDV con DDT e/o PDF del mittente
121500130411     c                   exsr      CercaDDToPDF
121600131206      *
121700131206      * Controllo se già effettuato il 1° passaggio c/o destinatario
121800131206     c                   exsr      sr_Check1pass
121900070319      *
122000070319      * Verifica se richiesta spedizione già parzialmente consegnata
122100070319      *   (da stampare nel box "Note per l'autotrasportatore")
122200070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
122300070717      * completamente
122400110203if  1c                   if             wB0dcp   >  *zeros
122500101220     c                             and  $GeoAtt  =  *on
122600070413     c                   eval      $ConsParz     =  *on
122700070319e   1c                   endif
122800030515      *
122900040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
123000040607      *   riserve)
123100120524      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
123200040223     c                   movea     '1000'        *in(70)
123300040311      *
123400030515sel 1c                   SELECT
123500071213      *
123600170517      * LDV DPD?(dalla fil. del mittente, NON della LNP)?
123700071213w   1c                   WHEN      §CMntw = 'DPD'
123800071213     c                   movea     '0100'        *in(70)
123900071213      *
124000071213      * LDV con mittente FedEx
124100150430w   1c                   WHEN      DB0ccm = c_CliFedEx   or
124200150430     c                             DB0ccm = c_CliFedEx2
124300071213     c                   movea     '0010'        *in(70)
124400030515      *
124500030515e   1c                   ENDSL
124600040707      *
124700040707      * Controllo se devo stampare "Non utilizzare per la consegna"
124800040707      * nella copia per il vettore,?senza?copia per il destinatario:
124900130924     c***?(già fatto:)?  reset                   $No_x_Cons
125000071203      * - LdV recapito c/assegni:
125100100322      *   stampata in arrivo e con
125200120113if  1c                   if             DB0lnp =  102
125300070306     c                             and  DB0cbo =  'FW'
125400071203     c                             and  DB0nft =  *zeros
125500071203      *   - fil. arrivo con GEO attivato
125600071203     c                             and (DB0fl1    =  'A'
125700071203     c                              or  wGeoCon   =  'S'
125800071203      *   - fil. arrivo abilitata al picking
125900120113     c                              or  A§OGpck   =  'S')
126000070306     c                   eval      $No_x_Cons  =  *on
126100120113e   1c                   endif
126200030515      *
126300030515     c                   ENDSR
126400040607      *
126500040607      *---------------------------------------------------------------*
126600130411      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
126700130411      *?               di uno o più PDF allegati da stampare         ?*
126800040607      *---------------------------------------------------------------*
126900130411     c     CercaDDToPDF  BEGSR
127000040607      *
127100130924     c***?(già fatto:)?  reset                   $FIAR8
127200130924     c***?(già fatto:)?  reset                   $§PDF
127300040607      *
127400130924     c***?(già fatto:)?  exsr      RepAR5gen
127500130411      *
127600130411      * -?Verifica esistenza PDF allegato?
127700130411     c                   eval      $§PDF  = (§AR5als = 'S')
127800130411      *
127900130411      * -?Verifica esistenza packing-list?
128000130411     c                   eval      $FIAR8 = (§AR5alx = 'S')
128100040607      *
128200080505     c                   ENDSR
128300131206      *
128400131206      /free
128500131206
128600131206       //--------------------------------------------------------------
128700131206       //?Verifica se già effettuato il 1° passaggio.                  ?
128800131206       //--------------------------------------------------------------
128900131206       BEGSR  sr_Check1pass;
129000131206
129100131206         //reset  $1o_Passaggio       ?(già fatto)?
129200131206
129300131206         // -?Apertura archivio?
129400131206         If  NOT  %open(FNEVB04L);
129500131206           open(e)  FNEVB04L;
129600131206           if  %error;
129700131206             exsr  sr_ErrOpenFile;
129800131206           endif;
129900131206         EndIf;
130000131206
130100131206         // -?Ciclo di lettura per ricercare l'eventuale 1° passaggio?
130200131210         setgt   %kds(keyFNEVB04 : 4)  FNEVB000;
130300131210         readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
130400131206
130500131206         DoW  Not %eof(FNEVB04L)  and  Not $1o_Passaggio;
130600131206
130700131206           dEVB01 = EVBnot;
130800131206           $1o_Passaggio = (dEVB01.§NOTprg = 'P');
130900131206
131000131206           if  Not $1o_Passaggio;
131100131210             readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
131200131206           endif;
131300131206
131400131206         EndDo;
131500131206
131600131206       ENDSR;
131700131206
131800131206      /end-free
131900030515      *
132000030515      *---------------------------------------------------------------*
132100050217      *?Imposta i campi di stampa con quelli della DS                ?*
132200030515      *---------------------------------------------------------------*
132300110208     c     sr_PrepPRT    BEGSR
132400040223      *
132500040223      * Reperimento dati da FIAR5 / "GEN"
132600130924     c***?(già fatto:)?  exsr      RepAR5gen
132700070212     *** *
132800070212     *** * Impostazione Data spedizione
132900110208     *** * (già fatto nella subr. "sr_TipoLdV")
133000030515      *
133100030515      * - Intestazione moduli
133200030515      *
133300030515      * - Sigla operatore video
133400030515     c                   eval      Psop   =  DB0sop
133500081120      /free
133600081120         //
133700130411         // -?Flag "allega documento di trasporto"?
133800081120         select;
133900081120
134000130411           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
134100081120           when  DB0bam =  'S'   and   wB0tmc <> *blank;
134200081120             evalr Pddt =  'DDT sul collo';
134300081120
134400130411           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
134500081120           when  DB0bam =  'S';
134600081120             evalr Pddt =  'DDT';
134700081120
134800130716           // -?L'indicazione di packing-list è stata spostata in PPKL?
134900130716           //// -?Esiste packing-list abbinata?
135000130411           //when  $FIAR8 =  *on   and   $§PDF =  *off;
135100131203           //  evalr Pddt =  'PACKING-LIST';
135200130716           //// -?LdV con PDF allegato?
135300130411           //when  $FIAR8 =  *off  and   $§PDF =  *on;
135400130411           //  evalr Pddt =  'PACKING-LIST PDF';
135500130716           //// -?LdV con packing-list abbinata & PDF allegato?
135600130411           //when  $FIAR8 =  *on   and   $§PDF =  *on;
135700130411           //  evalr Pddt =  'PACKING-LIST PDF';
135800130411
135900130411           // -?LdV senza PDF allegato & senza packing-list abbinata?
136000081120           other;
136100081120             clear Pddt;
136200081120
136300081120         endsl;
136400081120
136500081120      /end-free
136600030515      *
136700030515      * - Mittente
136800170517     c                   if        *in71
136900170517     c                   exsr      sr_Mitt_DPD
137000170517     c                   endif
137100170517      *
137200030515     c                   if        DB0ccm <> *zeros
137300030515     c                   move      DB0ccm        Pccm
137400030515     c                   endif
137500030515     c                   eval      Prsm   =  DB0rsm
137600030515     c                   eval      Pinm   =  DB0inm
137700030515     c                   eval      Pcam   =  DB0cam
137800030515     c                   eval      Plom   =  DB0lom
137900030515     c                   eval      Pprm   =  DB0prm
138000030515     c                   eval      Pnzm   =  DB0nzm
138100030515      *   Mittente originale
138200030515if  1c                   if        DB0rmo <> *blanks
138300080430     c                   eval      Pkmo   =  'Mitt.Originale '
138400030515     c                   eval      Prmo   =  DB0rmo
138500030515e   1c                   endif
138600030515      *
138700070308      * - Filiale di Arrivo
138800030515     c                   move      DB0lna        Plna
138900030515      *
139000030515      * - Destinatario
139100030515if  1c                   if        DB0ccd <> *zeros
139200030515     c                   move      DB0ccd        Pccd
139300030515e   1c                   endif
139400030515     c                   eval      Prsd   =  DB0rsd
139500030515     c                   eval      Prse   =  DB0rd2
139600030515     c                   eval      Pind   =  DB0ind
139700030515     c                   eval      Pcad   =  DB0cad
139800030515     c                   eval      Plod   =  DB0lod
139900030515     c                   eval      Pprd   =  DB0prd
140000030515     c                   eval      Pnzd   =  DB0nzd
140100030515      *
140200030515      * - Codice tariffa
140300030515if  1c                   if        %subst(DB0tb1:1:1) = 'A'
140400030515      *   - nel box del destinatario se porto assegnato
140500030515     c                   move      DB0ctr        Pcta
140600030515x   1c                   else
140700030515      *   - nel box del mittente se porto franco
140800030515     c                   move      DB0ctr        Pctf
140900030515e   1c                   endif
141000030515      *
141100030515      * - Riferimenti mittente
141200080430     c                   evalr     Prmn   =  %char(DB0rmn)
141300030515     c                   eval      Prma   =  DB0rma
141400171117      *//* se bolla DPD recupero il riferimento da FIAR4 rcd "I"
141500171117      * se bolla DPD recupero il riferimento da FIPND00F
141600171117     c*//***?(già fatto:)?  clear                   dsBL4I
141700060626     c                   if        *in71
141800171117     c*//                exsr      RepAR4_I
141900171117     c                   exsr      RepFIPND
142000060626     c                   endif
142100030515      *
142200030515      * - Nominativo & telefono
142300031124      *   (da FIAR5 / "GEN")
142400130924     c***?(già fatto:)?  exsr      RepAR5gen
142500031124     c                   eval      Pnom   =  §AR5ref
142600031124     c                   eval      Ptel   =  §AR5teld
142700030515      *
142800030515      * - Spedizione
142900030515      *   (numero: non se Rimesso al Mittente)
143000030515if  1c                   if        DB0ris <> 'M'
143100080430     c                   eval      Plnp   =  %editc(DB0lnp : 'X')
143200080430     c                   eval      Pnrs   =  %editc(DB0nrs : 'Z')
143300080430     c                   eval      Pnsp   =  %editc(DB0nsp : 'Z')
143400030515e   1c                   endif
143500030515      * . . Data spedizione
143600031124     c     *iso          movel     Wdsp          Data_EUR
143700080506     c                   eval      PdspA = %editc( %dec(Data_EUR) : 'Y' )
143800030515      *
143900030515      * - Colli: numero, peso, volume, flag tipo-volume
144000080430     c                   eval      PnclA  =  %editc(DB0ncl : 'Z')
144100080911      /free
144200080911           if  NOT *in71;
144300080911      /end-free
144400080430     c                   eval      PpkfA  =  %editc(DB0pkf : '4')
144500080430     c                   eval      PvlfA  =  %editc(DB0vlf : '4')
144600030515if  1c                   if            DB0fvf <> *blanks
144700030515     c                             and DB0ris <> 'M'
144800081021     c                   movel     DB0fvf        Pfvf
144900030515e   1c                   endif
145000080915      /free
145100080915           endif;
145200080915      /end-free
145300030515      *
145400030515      * - Natura merce
145500030515     c                   eval      Pnat   =  DB0nas
145600030515      *
145700030515      * - Servizio
145800100517      /free
145900100517         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
146000110117           exsr  sr_CheckORM_PreavvMail;
146100100517         endif;
146200100517      /end-free
146300100517      * . . Descrizione porto
146400130925      *     (NON se presente ORM con preavviso mail)
146500100517     c                   if        Not  $PreavvORM
146600030515     c                   eval      Pds1   =  DB0dcb
146700100517     c                   endif
146800030515      * . . Descrizione tipo servizio
146900030515if  1c                   if        DB0fst =  'S'
147000030515     c                   eval      Pds2   =  DB0dts
147100030515e   1c                   endif
147200030515      * . . Reso / Franco frontiera
147300030515sel 1c                   select
147400030515w   1c                   when      DB0fbr =  'S'
147500030515     c                   clear                   w008a
147600030515     c                   eval      w008a  =  '   RESO '
147700030515     c                   move      w008a         Pds2
147800030515w   1c                   when      DB0fbr =  'F'
147900030515     c                   clear                   w015a
148000030515     c                   eval      w015a  =  '   FR.FRONTIERA'
148100030515     c                   move      w015a         Pds2
148200030515e   1c                   endsl
148300030515      *
148400080430      * - Zona consegna
148500080430      *   (campo alfanumerico)
148600080430     c                   eval      Pznc   =  %editc(DB0znc : 'X')
148700030515      *
148800030515      * - Flag "*" (gestito a parte)
148900040223      *
149000040223      * - Flag "V" = colli di valore
149100040223if  1c                   if        §AR5bva   <> *blank
149200040223     c                   eval      Pbva   =  'V'
149300040223e   1c                   endif
149400030515      *
149500030515      * - Fermo deposito
149600030515if  1c                   if        DB0ffd <> *blanks
149700080430     c                   eval      Pgc2   =  'FERMO DEPOSITO '
149800030515e   1c                   endif
149900030515      *
150000070308      * - Segnacolli: filiale, serie, dal / al
150100030515if  1c                   if        DB0fls <> *zeros
150200080430     c                   eval      Ppos   =  %editc(DB0fls : 'X')
150300030515e   1c                   endif
150400040607      * - - se bolla con mamma considero la serie della mamma
150500070515if  1c                   if        $Mamma =  *on
150600080430     c                   eval      Psrs   =  %editc(wB0nrs : 'Z')
150700031015x   1c                   else
150800080430     c                   eval      Psrs   =  %editc(DB0nrs : 'Z')
150900031015e   1c                   endif
151000030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
151100030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
151200030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
151300030515      *   Se collo singolo .......:   "     SCD
151400031015sel 1c                   select
151500030515      * - - non sequenziali
151600030515w   1c                   when      DB0fns =  'S'
151700131008if  2c                   if        DB0ncl <= 2
151800050511     c                   exsr      ListaScNSd
151900131008x   2c                   else
152000081021     c                   eval      Pnsx   =  'NON SEQUENZIALI'
152100131008e   2c                   endif
152200030515      * - - singolo o sequenziali
152300030515      *     (non se rimesso al mittente)
152400030515w   1c                   when      DB0ris <> 'M'
152500080430     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
152600030515if  2c                   if            DB0nca <> *zeros
152700030515     c                             and DB0nca <> DB0ncd
152800080430     c                   eval      Pnsx   =  %trimr(Pnsx) + ' - '
152900080430     c                                    +  %editc(DB0nca : 'Z')
153000030515e   2c                   endif
153100030515e   1c                   endsl
153200030515      *
153300131016      * - Note (1 e 2) e Particolarità (3, 4, 5 e 6)
153400030515     c                   exsr      SchPARTIC
153500131016e   1 *
153600030716if  1c                   if           $NP(1) <> *blanks
153700030716     c                             or $NP(2) <> *blanks
153800030716     c                             or $NP(3) <> *blanks
153900030716     c                             or $NP(4) <> *blanks
154000030716     c                             or $NP(5) <> *blanks
154100030716     c                             or $NP(6) <> *blanks
154200030710     c                   eval      Pnp1   =  $NP(1)
154300030710     c                   eval      Pnp2   =  $NP(2)
154400030710     c                   eval      Pnp3   =  $NP(3)
154500030710     c                   eval      Pnp4   =  $NP(4)
154600030710     c                   eval      Pnp5   =  $NP(5)
154700030710     c                   eval      Pnp6   =  $NP(6)
154800030716e   1c                   endif
154900030515      *
155000080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
155100080401      *   (servizio consegna garantita x cliente Wuerth)
155200170413if  1c*//                if        DB0gva =  'P '
155300170413     c*//                movel     'PRIVATO'     Ppad
155400170413e   1c*//                endif
155500041012if  1c                   if        DB0gva =  'U '
155600041013     c                   eval      Ppad = 'CONSEGNA AD UFFICI'
155700041012e   1c                   endif
155800030515if  1c                   if           DB0tc1 =  'A'
155900030515     c                             or DB0tc2 =  'A'
156000030515     c                   move      'APPUNTAMENTO'Ppad
156100030515e   1c                   endif
156200080401if  1c                   if             DB0nrs = 40
156300090518     c                             and  DB0tsp = 'E'
156400080401     c                             and (DB0lnp = 043
156500080401     c                              or  DB0lnp = 006
156600080401     c                              or  DB0lnp = 063)
156700090304     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
156800090304     c                                     WÜRTH'
156900080401e   1c                   endif
157000030515      *
157100030515      * - Data e ora consegna richiesta
157200030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
157300091116      *   (NO "Prima del" per il cliente TTL)
157400091116if  0c                   if        DB0ccm <> c_CliTTL  or
157500091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
157600030515if  1c                   if        DB0dcr <> *zeros
157700030515     c                   eval      Past   =  '*'
157800030515     c     *iso          movel     DB0dcr        Data_EUR
157900030515     c     *eur          movel     Data_EUR      Pdcr
158000030515e   1c                   endif
158100030515if  1c                   if        DB0hcr <> *zeros
158200030515     c                   eval      Past   =  '*'
158300080430     c                   eval      Phcr   =  %editw(DB0hcr : '0 :  ')
158400030515e   1c                   endif
158500091116e   0c                   endif
158600030515      *
158700030515      * - Consegne particolari
158800030515     c                   eval      Pcp1   =  DB0dc1
158900030515     c                   eval      Pcp2   =  DB0dc2
159000030515      *
159100030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
159200030515      *   (importi "arrotondati" a 2 decimali in stampa)
159300030515if  1c                   if        DB0ift <> *zeros
159400080515     c                   eval      Pkim   = 'Imponibile'
159500080430     c                   eval(h)   w0112  =  DB0imv
159600080430     c                   eval      Pimp   =  %editc(w0112 : 'K')
159700030515sel 2c                   select
159800030515w   2c                   when      DB0iva <> *zeros
159900080515     c                   eval      Pkie   = 'I.v.a.    '
160000030624     c                   eval(h)   w0112  =  DB0iva
160100080430     c                   eval      Piva   =  %editc(w0112 : 'K')
160200030515w   2c                   when         DB0cei <> *blanks
160300030515     c                             or DB0dce <> *blanks
160400130213     c                   eval      Pkie   = 'Esenzione '
160500130213     c                   eval      Pese   = '           ' +
160600130213     c                                      %subst(DB0dce : 1 : %len(§EIdes))
160700030515e   2c                   endsl
160800080515     c                   eval      Pktf   = 'Tot. Fatt.'
160900080430     c                   eval(h)   w0112  =  DB0ift
161000080430     c                   eval      Pift   =  %editc(w0112 : 'K')
161100030515if  2c                   if        DB0div <> §GEDcn
161200030515     c                   eval      Pdiv   =  DB0div
161300030515e   2c                   endif
161400030515e   1c                   endif
161500030515      *
161600030515      * - Contrassegno e relativa divisa
161700030515if  1c                   IF        DB0cas <> *zeros
161800030625      *
161900080430     c                   eval(h)   w0112  =  DB0cas
162000080430     c                   eval      Pcas   =  %editc(w0112 : 'K')
162100030515if  2c                   if        DB0vca <> §GEDcn
162200030515     c                   eval      Pdca   =  DB0vca
162300030515e   2c                   endif
162400030515     c                   exsr      CtrCAs
162500030624      * - - Nuovo controllo abilitazione c/assegno
162600030624     c                   reset                   TRUL21DS
162700030624     c                   eval      I21cbo =  DB0cbo
162800030624     c                   eval      I21tsp =  DB0tsp
162900030624     c                   eval      I21lnp =  DB0lnp
163000030624     c                   eval      I21nzm =  DB0nzm
163100030624     c                   eval      I21lna =  DB0lna
163200030624     c                   eval      I21nzd =  DB0nzd
163300130826     c                   if        DB0ccm >  *zero
163400130826     c                   eval      I21ksc =  DB0ccm
163500130826     c                   else
163600130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
163700130826     c                   endif
163800030624     c                   eval      I21tic =  DB0tic
163900030624if  2c                   if        DB0vca <> §GEDcn
164000030624     c                   eval      I21imp =  Wcas
164100030624     c                   eval      I21div =  §GEDcn
164200030624x   2c                   else
164300030624     c                   eval      I21imp =  DB0cas
164400030624     c                   eval      I21div =  DB0vca
164500030624e   2c                   endif
164600080430     c                   eval      I21ute =  SDSusr
164700040607     c                   eval      I21pgm =  SDSpgm
164800030624     c                   call      'TRUL21R'
164900030624     c                   parm                    TRUL21ds
165000030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
165100030624     c                   exsr      Tab_1A
165200030624     c                   movel     §1Ades        DB0dti
165300030624if  2c                   if            §1Adeo <> *blanks
165400030624     c                             and O21fca <> *blanks
165500030624     c                   movel     §1Adeo        DB0dti
165600030624e   2c                   endif
165700030624      * - - Imposto i campi di stampa
165800080430     c                   eval      Pcti   =  DB0tic
165900080430     c                   eval      Ptic   =  DB0dti
166000030625      *
166100030515e   1c                   ENDIF
166200030515      *
166300030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
166400030515if  1c                   IF        DB0ift >  *zeros
166500030625      *
166600030515if  2c                   if            DB0vca <> §GEDcn
166700030515     c                             and DB0vca <> *blanks
166800030515     c                   eval      Pdie   =  DB0div
166900030515e   2c                   endif
167000030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
167100030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
167200030515      *   (importi "arrotondati" a 2 decimali in stampa)
167300030515sel 2c                   Select
167400110204      *   Non ho C/Assegno
167500101213w   2c                   When      DB0cas  = *zeros
167600030624     c                   eval(h)   w0112  =  DB0ift
167700030624     c                   eval      Pies   =  %editc(w0112 : 'K')
167800030625     c                   clear                   Pie1
167900030625     c                   clear                   Pdi1
168000030515      *   C/Assegno sommabile
168100030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
168200030515sel 3c                   select
168300030515      *   - c/assegno in EURO
168400030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
168500030624     c                   eval(h)   w0112  =  DB0ift + DB0cas
168600030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168700030624     c                   clear                   Pie1
168800030624     c                   clear                   Pdi1
168900030515      *   - c/assegno in DIVISA
169000030515w   3c                   when          DB0div =  §GEDcn
169100030515     c                             and DB0vca <> §GEDcn
169200030624     c                   eval(h)   w0112  =  DB0ift + Wcas
169300030624     c                   eval      Pies   =  %editc(w0112 : 'K')
169400030624     c                   clear                   Pie1
169500030624     c                   clear                   Pdi1
169600030515e   3c                   endsl
169700100218      *   C/Assegno non sommabile
169800030515x   2c                   Other
169900030624     c                   eval(h)   w0112  =  DB0cas
170000030624     c                   eval      Pie1   =  %editc(w0112 : 'K')
170100100218     c*//                eval      Pdi1   =  DB0vca
170200030624     c                   eval(h)   w0112  =  DB0ift
170300030624     c                   eval      Pies   =  %editc(w0112 : 'K')
170400100218     c*//                eval      Pdie   =  DB0div
170500030515e   2c                   Endsl
170600030709      *
170700030709e   1c                   ENDIF
170800030515      *
170900030515      * - Numero e data fattura
171000030515if  1c                   if        DB0nft <> *zeros
171100080430     c                   eval      Pkft   =  'Fatt.'
171200080430     c                   eval      Pnft   =  %editc(DB0nft : 'Z')
171300130205     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
171400130205     c                                      ')'
171500030515if  2c                   if        DB0dft >  *zeros
171600080430     c                   eval      Pkdf   =  ' del '
171700030515     c     *iso          movel     DB0dft        Data_EUR
171800130213     c     *dmy          movel     Data_EUR      Pdft
171900030515e   2c                   endif
172000030515e   1c                   endif
172100030515      *
172200030515      * Memorizzo il codice tariffa come numero di copie da stampare
172300030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
172400030515if  1c                   if        DB0ris = 'M'  and  *in70
172500030515     c                   eval      Wctr   =  DB0ctr
172600030515e   1c                   endif
172700030515      *
172800030515     c                   ENDSR
172900170517      *
173000170517      /free
173100170517       //--------------------------------------------------------------
173200170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
173300170517       //?spedizione DPD.                                              ?
173400170517       //--------------------------------------------------------------
173500170517       BEGSR  sr_Mitt_DPD;
173600170517
173700170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
173800170517         clear  dCLI;
173900170517         reset  TIBS02ds;
174000170517         T02cod = 'CLI';
174100170517         T02ke1 = %editc( DB0ccm : 'X' );
174200170517         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
174300170517         if  T02err = *blank;
174400170517           dCLI = T02uni;
174500170517         endif;
174600170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
174700170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
174800170517           leavesr;
174900170517         endif;
175000170517
175100170517         // -?Forzatura Mittente da note "AM"?
175200170517         If  §CLItfntc = 'S';
175300170517
175400170517           if  NOT %open( TFNTC01L );
175500170517             open  TFNTC01L;
175600170517           endif;
175700170517           clear  keyTFNTC01;
175800170517           k_NTCapl = 'C';
175900170517           k_NTCnk1 = %editc( c_Kci : 'X' ) + %editc( DB0ccm : 'X' );
176000170517           k_NTCtnt = 'AM';
176100170517           chain  %kds( keyTFNTC01 )  TFNTC;
176200170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
176300170517             DB0rsm = NTCrnt;
176400170517           endif;
176500170517
176600170517         EndIf;
176700170517
176800170517         // -?Pulizia Indirizzo Mittente?
176900170517         If  §CLInoIMtt = 'S';
177000170517
177100170517           clear  DB0inm;
177200170517           clear  DB0cam;
177300170517           clear  DB0lom;
177400170517           clear  DB0prm;
177500170517           clear  DB0nzm;
177600170517
177700170517         EndIf;
177800170517
177900170517       ENDSR;
178000170517
178100170517      /end-free
178200030515      *
178300030515      *---------------------------------------------------------------*
178400050217      *?Inschieramento delle Note e delle Particolarità              ?*
178500030515      *---------------------------------------------------------------*
178600030515     c     SchPARTIC     BEGSR
178700030515      *
178800130924     c***?(già fatto:)?  reset                   $NoDesPartic
178900130924     c***?(già fatto:)?  clear                   $NP
179000130924     c***?(già fatto:)?  clear                   W_CoOr
179100030515     c                   clear                   XX
179200030710      *
179300030710      * Imposto eventuali note per prime
179400030710if  1c                   if        DB0nt1 <> *blanks
179500030710     c                   add       1             xx
179600030710     c                   movel     DB0nt1        $NP(xx)
179700030710e   1c                   endif
179800030710if  1c                   if        DB0nt2 <> *blanks
179900030710     c                   add       1             xx
180000030710     c                   movel     DB0nt2        $NP(xx)
180100030710e   1c                   endif
180200030515      *
180300030515      * Le particolarità possono essere lunghe fino a 45 char.
180400030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
180500030515      *   bastare, il resto lo si mette nella riga successiva.
180600030710      *   Se, invece, non risultano decodificate, si mettono solo i
180700030710      *   codici sulla stessa riga.
180800030710      *   Se uno solo risulta decodificato, esso va su una riga a
180900030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
181000030710      *                                    *on  = ok riga procedente)
181100030515      *
181200030515      * . . Particolarità Giacenza
181300030515if  1c                   IF        DB0gga <> *blanks
181400030515     c                   movel     'Q'           WTpPar
181500030515     c                   movel     DB0gga        WCdPar
181600030515     c                   exsr      TABxPAR
181700030710if  2c                   if            TBLuni       = *blanks
181800030710     c                             and $NoDesPartic = *on
181900030710     c                   eval      $NP(xx) = %trim($NP(xx))
182000030710     c                                     + '   ' + WCdPar
182100030710x   2c                   else
182200030710if  3c                   if        TBLuni       = *blanks
182300030710     c                   eval      $NoDesPartic = *on
182400030710x   3c                   else
182500030710     c                   eval      $NoDesPartic = *off
182600030710e   3c                   endif
182700030515     c                   add       1             xx
182800100517     c                   movel     w053a         $NP(xx)
182900100517if  3c                   if        %subst(w053a : %len($NP(xx))+1)
183000030515     c                              <> *blanks
183100030515     c                   add       1             xx
183200030515     c                   eval      $NP(xx) = '   ' +
183300100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
183400030710e   3c                   endif
183500030515e   2c                   endif
183600030515e   1c                   ENDIF
183700030515      *
183800030515      * . . Particolarità Consegna
183900030515if  1c                   IF        DB0gma <> *blanks
184000030515     c                   movel     'R'           WTpPar
184100030515     c                   movel     DB0gma        WCdPar
184200030515     c                   exsr      TABxPAR
184300030710if  2c                   if            TBLuni       = *blanks
184400030710     c                             and $NoDesPartic = *on
184500030710     c                   eval      $NP(xx) = %trim($NP(xx))
184600030710     c                                     + '   ' + WCdPar
184700030710x   2c                   else
184800030710if  3c                   if        TBLuni       = *blanks
184900030710     c                   eval      $NoDesPartic = *on
185000030710x   3c                   else
185100030710     c                   eval      $NoDesPartic = *off
185200030710e   3c                   endif
185300030515     c                   add       1             xx
185400100517     c                   movel     w053a         $NP(xx)
185500100517if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
185600030515     c                              <> *blanks
185700030515     c                   add       1             xx
185800030515     c                   eval      $NP(xx) = '   ' +
185900100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
186000030710e   3c                   endif
186100030515e   2c                   endif
186200030515e   1c                   ENDIF
186300030515      *
186400030515      * . . Particolarità Varie
186500030717      *     - questa descrizione va messa nell'apposito campo PPAD
186600041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
186700030717      *     - se 'B' (bancali da rendere al mittente) comparirà un
186800030717      *       apposito box
186900030717      *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
187000030717      *       comparirà un apposito box, un'apposita nota e l'indica-
187100030717      *       zione nella copia per il destinatario.
187200030717      *     ...Potrebbero anche non esserci più elementi di schiera
187300030717      *     disponibili [xx >= %elem($NP)] !
187400030709if  1c                   IF            DB0gva <> *blanks
187500030717     c                             and DB0gva <> 'B '
187600030717     c                             and DB0gva <> 'O '
187700030709     c                             and DB0gva <> 'P '
187800041015     c                             and DB0gva <> 'U '
187900030515     c                   movel     'S'           WTpPar
188000030515     c                   movel     DB0gva        WCdPar
188100030515     c                   exsr      TABxPAR
188200030710if  2c                   if            TBLuni       = *blanks
188300030710     c                             and $NoDesPartic = *on
188400030710     c                   eval      $NP(xx) = %trim($NP(xx))
188500030710     c                                     + '   ' + WCdPar
188600030710x   2c                   else
188700030710if  3c                   if        TBLuni       = *blanks
188800030710     c                   eval      $NoDesPartic = *on
188900030710x   3c                   else
189000030710     c                   eval      $NoDesPartic = *off
189100030710e   3c                   endif
189200030717if  3c                   if            xx < %elem($NP)
189300030709     c                   add       1             xx
189400100517     c                   movel     w053a         $NP(xx)
189500100517if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
189600030709     c                              <> *blanks
189700030717     c                             and xx < %elem($NP)
189800030709     c                   add       1             xx
189900030709     c                   eval      $NP(xx) = '   ' +
190000100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
190100030717e   4c                   endif
190200030717e   3c                   endif
190300030709e   2c                   endif
190400030515e   1c                   ENDIF
190500030717      *
190600030717      * Imposto il numero originale di colli componenti la spedizione
190700030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
190800030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
190900030717     c                   exsr      RepAR5bnb
191000030717if  2c                   if        W_CoOr  <> *blanks
191100030717     c                             and xx  <  %elem($NP)
191200030717     c                   add       1             xx
191300030717     c                   eval      $NP(xx) =  W_CoOr
191400030717e   2c                   endif
191500030717e   1c                   endif
191600030515      *
191700030515     c                   ENDSR
191800070319      *
191900070319      *---------------------------------------------------------------*
192000070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
192100070319      *---------------------------------------------------------------*
192200070319     c     SchConsParz   BEGSR
192300070319      *
192400130924     c***?(già fatto:)?  reset                   $CP
192500130924     c***?(già fatto:)?  reset                   $CPdcm
192600070319     c                   clear                   XX
192700070319      *
192800070319if  2c                   if        NOT %open(FNART01L)
192900101213     c                   open(e)   FNART01L
193000101213      /free
193100101213             if  %error;
193200101213               exsr  sr_ErrOpenFile;
193300101213             endif;
193400101213      /end-free
193500070319e   2c                   endif
193600070319      *
193700070514if  2c                   if        $Mamma = *off
193800070319     c     KLDV          setll     FNART000
193900070319     c     KLDV          reade     FNART000
194000070514x   2c                   else
194100070514     c     KLBL          setll     FNART000
194200070514     c     KLBL          reade     FNART000
194300070514e   2c                   endif
194400070319      *
194500070319do  1c                   DOW       NOT  %eof(FNART01L)
194600070319      *
194700070319if  2c                   If             ARTatb = *blanks
194800070319     c                             and  ARTdcm > *zeros
194900070319      *
195000070319     c                   eval      *in31  = *off
195100070319     c                   eval      xx     = 1
195200070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
195300070319if  3c                   if        NOT  *in31
195400070319     c                   eval      xx     = 1
195500070319     c     *zeros        lookup    $CPdcm(xx)                             31
195600070319if  4c                   if        *in31
195700140304     c*//                eval      $CP(xx)     = *zeros
195800140304     c                   clear                   Elem_CP
195900140304     c                   movel     Elem_CP       $CP(xx)
196000070319e   4c                   endif
196100070319e   3c                   endif
196200070319if  3c                   if        *in31
196300070319     c                   eval      $CPdcm(xx)  = ARTdcm
196400070319     c                   movel     $CP(xx)       Elem_CP
196500070319     c                   eval      WCPdcm      = ARTdcm
196600070319     c                   eval      WCPncl      = WCPncl + 1
196700070319     c                   movel     Elem_CP       $CP(xx)
196800070319e   3c                   endif
196900070319      *
197000070319e   2c                   EndIf
197100070319      *
197200070514if  2c                   if        $Mamma = *off
197300070319     c     KLDV          reade     FNART000
197400070514x   2c                   else
197500070514     c     KLBL          reade     FNART000
197600070514e   2c                   endif
197700070319      *
197800070319e   1c                   ENDDO
197900070319      *
198000140514if  0c                   If        xx     > *zero
198100140513      * Reperimento nominativo firmatario   &
198200140514     c                   exsr      sr_Firmatario
198300070319      * Ordinamento schiera per data
198400070319if  1c                   if        xx     > 1
198500070319     c                   sorta                   $CP
198600070319e   1c                   endif
198700140514e   0c                   EndIf
198800070319      *
198900070319     c                   ENDSR
199000140303      *
199100140303      /free
199200140303
199300140303       //--------------------------------------------------------------
199400140303       //?Reperimento nome firmatario                                  ?
199500140303       //--------------------------------------------------------------
199600140303       BEGSR  sr_Firmatario;
199700140303
199800140303if  1    if  NOT  %open(FNEVB22L);
199900140303           open  FNEVB22L;
200000140303e   1    endif;
200100140303
200200140303         clear  keyFNEVB22;
200300140303         k_EVB2aas = DB0aas;
200400140303         k_EVB2lnp = DB0lnp;
200500140303         k_EVB2nrs = DB0nrs;
200600140303         k_EVB2nsp = DB0nsp;
200700140303         k_EVB2cev = 'P  ';
200800140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
200900140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
201000140303         DoW  Not  %eof(FNEVB22L);
201100140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
201200140304             yy = %lookup( EVB2dev : $CPdcm );
201300140304             if  yy > *zero;
201400140304               Elem_CP = $CP(yy);
201500140303               if  WCPocm  <= EVB2hev;
201600140303                 WCPocm  = EVB2hev;
201700140303                 WCPfir  = EVB2dati;
201800140304                 $CP(yy) = Elem_CP;
201900140303               endif;
202000140303             endif;
202100140303           endif;
202200140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
202300140303         EndDo;
202400140303
202500140303       ENDSR;
202600140303
202700140303      /end-free
202800031124      *
202900031124      *---------------------------------------------------------------*
203000050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
203100031124      *---------------------------------------------------------------*
203200031124     c     RepAR5gen     BEGSR
203300031124      *
203400031124     c                   clear                   dAR5gen
203500031124      *
203600031124     c                   eval      AR5trd = 'GEN'
203700031127     c     K05AR501      chain     FIAR5000
203800031124      *
203900101213if  1c                   if            %found(FIAR501L)
204000040301     c                             and AR5atb =  *blanks
204100031124     c                   movel     AR5uni        dAR5gen
204200031124e   1c                   endif
204300031124      *
204400031124     c                   ENDSR
204500030717      *
204600030717      *---------------------------------------------------------------*
204700050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
204800030717      *---------------------------------------------------------------*
204900030717     c     RepAR5bnb     BEGSR
205000030717      *
205100030717     c                   clear                   dAR5bnb
205200130924     c***?(già fatto:)?  clear                   W_CoOr
205300030717      *
205400030717     c                   eval      AR5trd = 'BNB'
205500030717     c     K05AR501      chain     FIAR5000
205600030717      *
205700101213if  1c                   if            %found(FIAR501L)
205800040301     c                             and AR5atb =  *blanks
205900030717     c                   movel     AR5uni        dAR5bnb
206000030717if  2c                   if        §AR5bcor <> *zeros
206100030717     c                   eval      W_CoOr   =  C_CoOr
206200030717     c                                      +  %editc(§AR5bcor : 'K')
206300030717e   2c                   endif
206400030717e   1c                   endif
206500030717      *
206600030717     c                   ENDSR
206700030515      *
206800030515      *---------------------------------------------------------------*
206900051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
207000030515      *---------------------------------------------------------------*
207100110208     c     sr_DecodLDV   BEGSR
207200030515      *
207300030515      * Richiesta firma per ricevuta della merce senza riserve
207400030515      * (e NON  quella  per quietanza) => *in35 + not *in36
207500030515     c                   movea     '10'          *in(35)
207600070307      *
207700070307      * Intestazione moduli
207800090619      *
207900090619      /free
208000121018
208100121018         // -?Indicazione della Partiva IVA o del Cod. Fiscale del Destinatario?
208200121018         //  ?se Porto Assegnato con Fattura immediata?
208300121018         //  ?- NON se destinatario estero -?
208400121018             // -?con fattura?
208500121018         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
208600121018             // - -?P.A. (con fattura immediata)?
208700121018             //    ?(con destinatario italiano)?
208800121018             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
208900121018             // - -?con 2ª bolla?
209000121018             //    ?(con destinatario italiano)?
209100131203              (DB0tb2 <> *blank         and  DB0nzd  = *blank) );
209200121018           select;
209300131010             // ·?Esiste Partita Iva NON FITTIZIA?
209400131010             when  wB0piv <> *blank  and
209500121026                   %scan('$$' : wB0piv) = *zero;
209600121018               PpivD = 'Partita Iva: ' + wB0piv;
209700131010             // ·?Esiste Codice Fiscale?
209800131010             when  wB0cfi <> *blank;
209900121018               PpivD = 'Codice Fiscale: ' + wB0cfi;
210000121018           endsl;
210100121018         ENDIF;
210200110204
210300110204         // -?Consegna al mattino (tempi consegna = 12 ore)?
210400110204         //  ?(NON per Würth)?
210500110204         If  §VPOrcm <= wDate and
210600090619             DB0tsp   = 'E'   and
210700090619            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
210800090619                                   DB0lnp <> 006   and
210900090619                                   DB0lnp <> 063));
211000090619           clear  TNSD99ds;
211100101213           D98tbo = 'A';
211200090619           D98aas = DB0aas;
211300090619           D98lnp = DB0lnp;
211400090619           D98nrs = DB0nrs;
211500090619           D98nsp = DB0nsp;
211600090619           tnsd99r ( TNSD99ds );
211700120910           if  D98tcc = 12;
211800090619             %subst(Pds2 : %len(Pds2) - 1) = ' M';
211900090619           endif;
212000110204         EndIf;
212100110204
212200110204         // -?"NON UTILIZZARE PER LA CONSEGNA"?
212300110204         //  ?(stampa della sola copia per il vettore)?
212400110204         if  $No_x_Cons = *on;
212500110204           *in35 = *off;
212600110204           //*in36 = *off;
212700110204           // -?costante da stampare se LDV recapito c/assegni stampata?
212800110204           //  ?dal pgm "stampa DDT NO in arrivo"?
212900110204           //Pnux   =  ' NON UTILIZZARE PER LA CONSEGNA';
213000110204         endif;
213100110204
213200110204         // -?Decodifica "Riserva Consegna" (se c'è)?
213300110204         clear  ds2A;
213400110204         If  ILVriserv <> *blank;
213500110204           TBLcod = '2A';
213600110204           TBLkey = ILVriserv;
213700110207           chain  K03TAB  TABEL;
213800110204           if  %found(TABEL00F);
213900110204             ds2A = TBLuni;
214000110204           endif;
214100110204         EndIf;
214200140514
214300140514         // -?Reperimento consegne parziali precedenti da FNART?
214400140514         If  $ConsParz;
214500140514           exsr  SchConsParz;
214600140514           if  xx < 1;
214700140514             $ConsParz = *off;
214800140514           endif;
214900140514         EndIF;
215000110204
215100110204         // -?Costante "PER RICEVUTA DELLA MERCE..."?
215200130716         exsr  sr_PerRicevutaDellaMerce;
215300110204
215400090619      /end-free
215500030515      *
215600130411      * Verifico se Packing-List e/o PDF
215700130411     c                   select
215800130411     c                   when      $§PDF  = *on
215900130411     c                   eval      Ppkl   =  'PACKING LIST PDF'
216000130411     c                   when      $FIAR8 =  *on
216100130411     c                   eval      Ppkl   =  'PACKING LIST    '
216200130411     c                   endsl
216300030515      *
216400030515      * Flag "*" > x Consegne particolari
216500030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
216600030515     c                   eval      Past   =  '*'
216700030515     c                   endif
216800030515      *
216900030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
217000030515if  1c                   IF        DB0cas <> *zeros
217100030515      * - Flag "*" > x C/Assegno
217200030904if  2c                   if        O21fca <> *blanks
217300030904     c                   movel     '*'           Past
217400030515e   2c                   endif
217500030624      * - Imposto i campi di stampa
217600080430     c                   eval      Pcti   =  DB0tic
217700080430     c                   eval      Ptic   =  DB0dti
217800030515e   1c                   ENDIF
217900030515      *
218000040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
218100030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
218200030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
218300030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
218400030515     c                             or        DB0tb2     <> *blanks
218500030515     c                   eval      Pass   =  'A'
218600030515if  2c                   if        DB0nft =  *zeros
218700080430     c                   if        Pkie   =  *blanks
218800080430     c                   evalr     Pkie   =  'SEGUE'
218900080430     c                   eval      Piva   =  ' FATTURA'
219000080430     c                   else
219100080430     c                   eval      Piva   =  'SEGUE FATTURA'
219200080430     c                   endif
219300030515e   2c                   endif
219400030515e   1c                   endif
219500030515      *
219600030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
219700051129if  1c                   IF            DB0dcr >  *zeros
219800051129     c                             or  DB0hcr >  *zeros
219900051129     c                   clear                   FNLV80ds
220000051129     c                   eval      ILV80aas   =  DB0aas
220100051129     c                   eval      ILV80lnp   =  DB0lnp
220200051129     c                   eval      ILV80nrs   =  DB0nrs
220300051129     c                   eval      ILV80nsp   =  DB0nsp
220400051129     c                   eval      ILV80mgs   =  DB0mgs
220500051129     c                   eval      ILV80tc1   =  DB0tc1
220600051129     c                   eval      ILV80tc2   =  DB0tc2
220700051129     c                   eval      ILV80dcr   =  DB0dcr
220800051129     c                   eval      ILV80hcr   =  DB0hcr
220900051129     c                   eval      ILV80tcr   =  DB0tcr
221000051129     c                   call      'FNLV80R'
221100051129     c                   parm                    KPJBA
221200051129     c                   parm                    FNLV80ds
221300051129sel 2c                   Select
221400051129      * errore
221500051129w   2c                   When      OLV80err  <>  *blanks
221600051129      * consegna tassativa (per ora solo ai supermercati)
221700051129w   2c                   When      OLV80tat   =  'S'
221800081021     c                   movel     OLV80tcrd     Ptdr
221900091116      * NO "Prima del" per il cliente TTL
222000091116w   2c                   When      DB0ccm     = c_CliTTL   and
222100091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
222200051129      * prima/dopo il/delle
222300051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
222400110208      *  nella subr. "sr_TipoLDV")
222500081022x   2c                   Other
222600081021     c                   movel     OLV80tcrd     Ptdr
222700081021e   2c                   Endsl
222800051129e   1c                   ENDIF
222900030515      *
223000030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
223100060921      * (SE?non?è "Fermo Deposito")
223200030515      * 1° turno di chiusura
223300060921if  1c                   IF             DB0gc1 <> *blanks
223400060921     c                             and  DB0ffd  = *blanks
223500030515      * - Flag "*" > Turno di chiusura
223600030515     c                   eval      Past   =  '*'
223700030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
223800030515      * - Nessun giorno specificato - Solo 'M' o 'P'
223900130924     c***?già così:?     clear                   Pgc1
224000030515if  3c                   if        DB0gc1 =  ' M'
224100030515     c                   eval      Pgc1   =  %trim($Gio(8))
224200030515x   3c                   else
224300030515     c                   eval      Pgc1   =  %trim($Gio(9))
224400030515e   3c                   endif
224500030515x   2c                   Else
224600030515      * - Giorno specificato
224700030515     c                   movel     DB0gc1        w001a
224800030515     c                   eval      XX     =  1
224900030515     c                   eval      *in31  =  *off
225000030515     c     w001a         lookup    $GgS(xx)                               31
225100030515     c   31              movel     $Gio(xx)      Pgc1
225200030515      *   + 'M' o 'P'
225300030515sel 3c                   select
225400030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
225500030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
225600030515     c                                    +  %trim($Gio(8))
225700030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
225800030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
225900030515     c                                    +  %trim($Gio(9))
226000030515e   3c                   endsl
226100030515e   2c                   Endif
226200030515e   1c                   ENDIF
226300030515      * 2° turno di chiusura
226400060921if  1c                   IF             DB0gc2 <> *blanks
226500060921     c                             and  DB0ffd  = *blanks
226600030515      * - Flag "*" > Turno di chiusura
226700030515     c                   eval      Past   =  '*'
226800030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
226900030515      * - Nessun giorno specificato - Solo 'M' o 'P'
227000130924     c***?già così:?     clear                   Pgc2
227100030515if  3c                   if        DB0gc2 =  ' M'
227200080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
227300080515     c                                                = %trim($Gio(8))
227400030515x   3c                   else
227500080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
227600080515     c                                                = %trim($Gio(9))
227700030515e   3c                   endif
227800030515x   2c                   Else
227900030515      * - Giorno specificato
228000030515     c                   movel     DB0gc2        w001a
228100030515     c                   eval      XX     =  1
228200030515     c                   eval      *in31  =  *off
228300030515     c     w001a         lookup    $GgS(xx)                               31
228400080515     c   31              eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
228500080515     c                                                = %trim($Gio(xx))
228600030515      *   + 'M' o 'P'
228700030515sel 3c                   select
228800030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
228900080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
229000030515     c                                    +  %trim($Gio(8))
229100030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
229200080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
229300030515     c                                    +  %trim($Gio(9))
229400030515e   3c                   endsl
229500030515e   2c                   Endif
229600030515e   1c                   ENDIF
229700080523      /free
229800080523       // ________________________________________
229900080523       // Impostazione nominativo del destinatario
230000080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
230100110214         exsr  RepAR4_1;
230200080523       // _______________________________________
230300080523       // Impostazione data ed ora della consegna
230400080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
230500110228         //?ATTENZIONE:?il "RESET" di un campo di tipo "d" (data)?
230600110228         //?riporta in tale campo la *date, NON quella impostata?
230700110228         //?come "inz(...)" della specifica "d" !!!!!!!!!!!!!!!!?
230800110228         Data_EUR = *loval;
230900110209         clear  w0080;
231000110209         select;
231100110209           when  wB0dcm > *zero;
231200110209             Data_EUR = %date( wB0dcm : *iso );
231300110209           when  wB0dcp > *zero;
231400110209             Data_EUR = %date( wB0dcp : *iso );
231500110209         endsl;
231600110209         if  Data_EUR > *loval;
231700110209           w0080 = ( ( %subdt(Data_EUR : *days) * 1000000 ) +
231800110209                     ( %subdt(Data_EUR : *months) * 10000 ) +
231900110209                     ( %subdt(Data_EUR : *years) ) );
232000110209         endif;
232100150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
232200081103           Pfedkcd  = %editc( w0080 : 'Y' );
232300120702           if  dILVlibero.§ILVhmp > *zero;
232400081103             Pfedkcd = %trimr( Pfedkcd ) + '  '
232500120702                     + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
232600120702                     + %subst( dILVlibero.§ILVhmp : 3 : 2 );
232700081103           endif;
232800081103         else;
232900081103           Pkcd  = %editc( w0080 : 'Y' );
233000120702           if  dILVlibero.§ILVhmp > *zero;
233100081103             Pkcd = %trimr( Pkcd ) + '   '
233200120702                  + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
233300120702                  + %subst( dILVlibero.§ILVhmp : 3 : 2 );
233400081103           endif;
233500081103         endif;
233600080523      /end-free
233700071114      *
233800071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
233900030515      *
234000071114      * Se pgm richiamato x stampa LdV in arrivo:
234100030515if  1c                   IF        DB0fl1 =  'A'
234200071114      * - imposto l'"*" in base al parametro ricevuto.
234300030515if  2c                   if        DB0fl2 =  '*'
234400030515     c                   eval      Past   =  '*'
234500030515x   2c                   else
234600030515     c                   clear                   Past
234700030515e   2c                   endif
234800071114      * - idem per flag di bolla non abilitata
234900030515     c                   eval      Pdis   =  DB0fl5
235000030515e   1c                   endif
235100030515      *
235200030515     c                   ENDSR
235300080430      *
235400080430      /free
235500130716
235600130716       //--------------------------------------------------------------
235700130716       //?Impostazione "PER RICEVUTA DELLA MERCE ..."                  ?
235800130716       //--------------------------------------------------------------
235900130716       BEGSR  sr_PerRicevutaDellaMerce;
236000130716
236100130716         Select;
236200130716
236300130716           // -?danon?stampare se?
236400130716           // ·?non utilizzare per la consegna  (NOT *in35)?
236500130716           When  NOT *in35;
236600130716             //clear  Pkrm;       ?(già fatto!)?
236700140108             //clear  Pkrm2;      ?(già fatto!)?
236800130716
236900130716           // -?costante per "DPD"   (come standard)?
237000130716           //         ?o per "FedEx" (come standard)?
237100130716           When  *in71  or  *in72;
237200130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
237300130716             // -?senza riserve?
237400130716             if  ILVriserv = *blank;
237500130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
237600130716             // -?con Riserva Consegna?
237700130716             else;
237800130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
237900130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
238000130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
238100130716               endif;
238200130716             endif;
238300130716
238400130716           // -?costante per cliente?
238500130716           //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
238600140513           When  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
238700130716             exsr  sr_RepDdtCLN;
238800130716             Pkrm = 'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
238900130716                     DDT '
239000130716                  + %trimr( %editc (WddtCLN : 'Z') )
239100130716                  + ' DEL '
239200130716                  + %editc (WdspCLN : 'Y')
239300130716                  + ' CON RISERVA DI CONTROLLO COME CONTRATTO';
239400130716             if  Wdsp < 20101001;
239500130716               Pkrm = %trimr(Pkrm) + ' C.L.N.';
239600130716             else;
239700130716               Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
239800130716             endif;
239900130716
240000130716           // -?costante "standard" se "DDT sì"?
240100130716           When  §VPOrsr <= wDate  and  DB0bam = 'S';
240200130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
240300130716                     TRASPORTO';
240400130716             // -?senza riserve?
240500130716             if  ILVriserv = *blank;
240600130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
240700130716             // -?con Riserva Consegna?
240800130716             else;
240900130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
241000130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
241100130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
241200130716               endif;
241300130716             endif;
241400130716
241500130716           // -?costante "standard" se "DDT no" + Packing-List?
241600161012           When  DB0bam = *blank  and  ($FIAR8 = *on  or  $§PDF = *on);
241700130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
241800130716                     TRASPORTO';
241900130716             // -?senza riserve?
242000130716             if  ILVriserv = *blank;
242100130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
242200130716             // -?con Riserva Consegna?
242300130716             else;
242400130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
242500130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
242600130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
242700130716               endif;
242800130716             endif;
242900130716
243000130716           // -?costante "standard" altrimenti?
243100130716           Other;
243200130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
243300130716             // -?senza riserve?
243400130716             if  ILVriserv = *blank;
243500130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
243600130716             // -?con Riserva Consegna?
243700130716             else;
243800130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
243900130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
244000130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
244100130716               endif;
244200130716             endif;
244300130716
244400130716         EndSl;
244500130716
244600130716         // -?costante aggiuntiva per cliente?
244700130716         //  ?"S G Soluzioni Logistiche"?
244800130716         If  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
244900130716
245000130716           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
245100130716             Pkrm2 = 'INCLUDE CONTRATTO - '
245200130716                   + %trim(DB0nas);
245300130716           endif;
245400130716           if  DB0rmo <> *blank;
245500130716             if  Pkrm2 <> *blank;
245600130716               Pkrm2 = %trimr(Pkrm2)
245700130716                     + ' - MATR TV: '
245800130716                     + %trim(DB0rmo);
245900130716             else;
246000130716               Pkrm2 = 'MATR TV: '
246100130716                     + %trim(DB0rmo);
246200130716             endif;
246300130716           endif;
246400130716
246500130716         EndIf;
246600140108
246700140108         // -?Consegne Parziali Precedenti?
246800140108         If  $ConsParz = *on  and  wB0dcm = *zero;
246900140108
247000140108           Select;
247100140110             When  Pkrm2 = *blank;
247200140110               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
247300140108             When  %subst( Pkrm : 111 ) = *blank;
247400140108               Pkrm = %subst( Pkrm : 1 : 110 ) + ' PARZIALE';
247500140108             When  %subst( Pkrm2 : 111 ) = *blank;
247600140108               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
247700140108           EndSl;
247800140108
247900140108         EndIf;
248000130716
248100130716       ENDSR;
248200090619
248300090619       //--------------------------------------------------------------
248400130716       //?Reperimento Numero e Data del DDT originale del mittente     ?
248500130716       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
248600090619       //--------------------------------------------------------------
248700090619       BEGSR  sr_RepDdtCLN;
248800090619
248900130716         // -?Verifica se la spedizione ha mamma?
249000090619         if  not %open(FNLBL01L);
249100101213           open(e)  FNLBL01L;
249200101213           if  %error;
249300101213             exsr  sr_ErrOpenFile;
249400101213           endif;
249500090619         endif;
249600090619         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
249700090619
249800090619         Select;
249900090619
250000130716           // -?Con Mamma: reperimento dati DDT dalla mamma?
250100090619           When  %found(FNLBL01L);
250200090619             if  not %open(FNBLP01L);
250300101213               open(e)  FNBLP01L;
250400101213               if  %error;
250500101213                 exsr  sr_ErrOpenFile;
250600101213               endif;
250700090619             endif;
250800090619             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
250900090619             if  %found(FNBLP01L);
251000090619               exsr  sr_DatiCLN_blp;
251100090619             endif;
251200090619
251300130716           // -?In Arrivo: reperimento dati DDT da FNARB?
251400090619           Other;
251500090619             exsr  sr_DatiCLN_arb;
251600090619
251700090619         EndSl;
251800090619
251900090619       ENDSR;
252000090619
252100090619       //--------------------------------------------------------------
252200090619       //?In Partenza: reperimento dati DDT da FNBLP                   ?
252300090619       //--------------------------------------------------------------
252400090619       BEGSR  sr_DatiCLN_blp;
252500090619
252600130716         // -?Numero DDT?
252700090619         select;
252800090619           when  BLPrmn <> *zero;
252900100906             if  BLPrmn  > -99999999   and
253000100906                 BLPrmn <   99999999;
253100100906               WddtCLN = BLPrmn;
253200100906             endif;
253300090619           when  BLPncd <> *zero;
253400090619             WddtCLN = BLPncd;
253500090619         endsl;
253600090619
253700130716         // -?Data DDT in formato gg/mm/aaaa?
253800090619         Data_Dmy = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
253900100906         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
254000100906                      ( %subdt(Data_Dmy : *months) * 100 ) +
254100100906                ( %int( %subst( %editc(BLPaas : 'X') : 3 : 2 ) ) ) );
254200090619
254300090619       ENDSR;
254400090619
254500090619       //--------------------------------------------------------------
254600090619       //?In Arrivo: reperimento dati DDT da FNARB                     ?
254700090619       //--------------------------------------------------------------
254800090619       BEGSR  sr_DatiCLN_arb;
254900090619
255000130716         // -?Numero DDT?
255100090619         select;
255200090619           when  ARBrmn <> *zero;
255300100906             if  ARBrmn  > -99999999   and
255400100906                 ARBrmn <   99999999;
255500100906               WddtCLN = ARBrmn;
255600100906             endif;
255700090619           when  ARBncd <> *zero;
255800090619             WddtCLN = ARBncd;
255900090619         endsl;
256000090619
256100130716         // -?Data DDT in formato gg/mm/aaaa?
256200090619         Data_Dmy = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
256300090619         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
256400090619                      ( %subdt(Data_Dmy : *months) * 100 ) +
256500110117                ( %int( %subst( %editc(ARBaas : 'X') : 3 : 2 ) ) ) );
256600090619
256700090619       ENDSR;
256800100517
256900100517       //--------------------------------------------------------------
257000100517       //?Verifica se presente ORM con preavviso mail                  ?
257100100517       //--------------------------------------------------------------
257200110117       BEGSR  sr_CheckORM_PreavvMail;
257300100517
257400130924         //clear  dOrm01;             ?(già fatto)?
257500100517
257600130925         // -?Reperimento num. ORM?
257700130924         exsr  RepAR4_M;
257800100517
257900130925         // -?SE ORM inesistente: inutile proseguire?
258000130925         if  §B4poe = *zero   or   §B4nor = *zero;
258100100517           leavesr;
258200100517         endif;
258300100517
258400130716         // -?Verifica presenza O.R.M.?
258500100517         if  Not  %open(FNORM01L);
258600101213           open(e)  FNORM01L;
258700101213           if  %error;
258800101213             exsr  sr_ErrOpenFile;
258900101213           endif;
259000100517         endif;
259100100517
259200100517         ORMpoe = §B4poe;
259300100517         ORMnsr = §B4nsr;
259400100517         ORMnor = §B4nor;
259500100517         ORMnrv = §B4nrv;
259600100517
259700100517         chain  K04ORM01  FNORM000;
259800100517
259900130716         // -?ORM non reperito: inutile proseguire?
260000100517         if  Not  %found(FNORM01L);
260100100517           leavesr;
260200100517         endif;
260300100517
260400130716         // -?Impostazione flag per stampa porto "ASSEGNATO"?
260500100517         dOrm01 = ORMflo;
260600100517         $PreavvORM = (§ORMpre = 'M');
260700100517
260800100517       ENDSR;
260900090619
261000080430       //--------------------------------------------------------------
261100140303       //?Preparazione dati di output relativi alla Lettera di Vettura ?
261200080430       //--------------------------------------------------------------
261300131016       BEGSR  sr_DatiOUT;
261400080430
261500101221         // -?Impostazione righe?
261600080430         clear LdV;
261700080430         clear wx;
261800080430
261900101221         // -?Area immagine LdV?
262000080801
262100110203         FOR  wx = 1  TO  c_Row;
262200080430
262300080430           %occur(LdV) = wx;
262400080430
262500080508           LdvPrg      = wx;
262600080430           select;
262700080430
262800101221             // ·? 1ª riga?
262900110203             when  wx = 1;
263000110203             LdvTxt = ds_01;
263100080430
263200101221             // ·? 2ª riga (vuota)?
263300110203             when  wx = 2;
263400080430             clear LdvTxt;
263500080430
263600101221             // ·? 3ª riga?
263700110203             when  wx = 3;
263800110203             LdvTxt = ds_03;
263900080430
264000101221             // ·? 4ª riga?
264100110203             when  wx = 4;
264200110203             LdvTxt = ds_04;
264300080430
264400101221             // ·? 5ª riga?
264500110203             when  wx = 5;
264600110203             LdvTxt = ds_05;
264700080430
264800101221             // ·? 6ª riga (vuota)?
264900110203             when  wx = 6;
265000110203             clear LdvTxt;
265100080430
265200101221             // ·? 7ª riga?
265300110203             when  wx = 7;
265400110203             LdvTxt = ds_07;
265500080430
265600101221             // ·? 8ª riga (vuota)?
265700110203             when  wx = 8;
265800080430             clear LdvTxt;
265900080430
266000101221             // ·? 9ª riga?
266100110203             when  wx = 9;
266200110203             LdvTxt = ds_09;
266300080430
266400101221             // ·?10ª riga?
266500110203             when  wx = 10;
266600110203             LdvTxt = ds_10;
266700080430
266800101221             // ·?11ª riga?
266900110203             when  wx = 11;
267000110203             LdvTxt = ds_11;
267100080430
267200101221             // ·?12ª riga?
267300110203             when  wx = 12;
267400110203             LdvTxt = ds_12;
267500080430
267600101221             // ·?13ª riga?
267700110203             when  wx = 13;
267800110203             LdvTxt = ds_13;
267900080430
268000101221             // ·?14ª riga?
268100110203             when  wx = 14;
268200110203             LdvTxt = ds_14;
268300080430
268400101221             // ·?15ª riga?
268500110203             when  wx = 15;
268600110203             LdvTxt = ds_15;
268700080430
268800101221             // ·?16ª riga?
268900110203             when  wx = 16;
269000110203             LdvTxt = ds_16;
269100080430
269200101221             // ·?17ª riga?
269300110203             when  wx = 17;
269400110203             LdvTxt = ds_17;
269500080430
269600101221             // ·?18ª riga?
269700110203             when  wx = 18;
269800110203             LdvTxt = ds_18;
269900080430
270000101221             // ·?19ª riga?
270100110203             when  wx = 19;
270200110203             LdvTxt = ds_19;
270300080430
270400101221             // ·?20ª riga?
270500110203             when  wx = 20;
270600110203             LdvTxt = ds_20;
270700080430
270800101221             // ·?21ª riga?
270900110203             when  wx = 21;
271000110203             LdvTxt = ds_21;
271100080430
271200101221             // ·?22ª riga?
271300110203             when  wx = 22;
271400110203             LdvTxt = ds_22;
271500080430
271600101221             // ·?23ª riga?
271700110203             when  wx = 23;
271800110203             LdvTxt = ds_23;
271900080430
272000101221             // ·?24ª riga?
272100110203             when  wx = 24;
272200110203             LdvTxt = ds_24;
272300080430
272400101221             // ·?25ª riga?
272500110203             when  wx = 25;
272600110203             LdvTxt = ds_25;
272700080430
272800101221             // ·?26ª riga (vuota)?
272900110203             when  wx = 26;
273000080806             clear LdvTxt;
273100080430
273200101221             // ·?27ª riga?
273300110203             when  wx = 27;
273400110203             LdvTxt = ds_27;
273500080430
273600101221             // ·?28ª riga?
273700110203             when  wx = 28;
273800110203             LdvTxt = ds_28;
273900081021
274000101221             // ·?29ª riga?
274100110203             when  wx = 29;
274200081021             LdvTxt = ds_29;
274300081021
274400101221             // ·?30ª riga?
274500110203             when  wx = 30;
274600081021             LdvTxt = ds_30;
274700081021
274800110215             // ·?31ª riga?
274900110203             when  wx = 31;
275000110215             LdvTxt = ds_31;
275100081021
275200101221             // ·?32ª riga?
275300110203             when  wx = 32;
275400081021             LdvTxt = ds_32;
275500081021
275600101221             // ·?33ª riga?
275700110203             when  wx = 33;
275800081021             LdvTxt = ds_33;
275900081021
276000101221             // ·?34ª riga?
276100110203             when  wx = 34;
276200081021             LdvTxt = ds_34;
276300081021
276400101221             // ·?35ª riga (vuota)?
276500110203             when  wx = 35;
276600081021             clear LdvTxt;
276700081021
276800101221             // ·?36ª riga (vuota)?
276900110203             when  wx = 36;
277000081021             clear LdvTxt;
277100080430
277200080430           endsl;
277300080430
277400080430         ENDFOR;
277500080430
277600110518         // -?Impostazione iniziale del modulo standard:?
277700131203         //// ·?Modulo standard con logo Bartolini?
277800131203         //TemplateName = 'ldvbase00 ';
277900131203         //// ·?Modulo standard con logo Bartolini-BRT?
278000131203         //TemplateName = 'ldvbase10 ';
278100120524         // ·?Modulo standard con logo BRT?
278200120524         TemplateName = 'ldvbase20 ';
278300080801
278400110203         Select;
278500081030
278600101221           // -?Area dati "DPD"?
278700101221           //  ?(NON della LNP, ma dalla fil. ricavata dal cod.mittente)?
278800110203           When  §CMntw = 'DPD';
278900110203             exsr  sr_LdVbaseDPD;
279000081030
279100101221           // -?Area dati "FedEx"?
279200150430           When  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
279300110203             exsr  sr_LdVbaseFED;
279400081030
279500131206           // -?Area box (ex "Promemoria per l'autotrasportatore")?
279600131206           When  $1o_Passaggio = *on  or  $ConsParz = *on;
279700110203             exsr  sr_LdVbase01;
279800081030
279900110203         EndSl;
280000130411
280100130411
280200130411         // ? CHIODI PER TEST                               ?
280300130411         Select;
280400130411           // -?Nessun TEST?
280500130411           When  $Test = *blank;
280600130411           // -?Spedizione FedEx?
280700130411           When  $Test = 'FED';
280800130411             TemplateName = 'ldvbase23 ';
280900130411           // -?Spedizione DPD?
281000130411           When  $Test = 'DPD';
281100130411             TemplateName = 'ldvbase22 ';
281200130411           // -?Spedizione con Consegna Parziale Precedente?
281300130411           When  $Test = 'C_P';
281400130411             TemplateName = 'ldvbase21 ';
281500130411           // -?Spedizione Normale?
281600130411           When  $Test = 'NOR';
281700130411             TemplateName = 'ldvbase20 ';
281800130411         EndSl;
281900130411
282000080801
282100101221         // -?Vuoti finali?
282200130213         For  wx = wx  To  w_Row;
282300080801           %occur(LdV) = wx;
282400130213           if  LdVPrg = *zero;
282500130213             LdvPrg = wx;
282600130213             clear  LdvTxt;
282700130213           endif;
282800110203         EndFor;
282900130205
283000130205         // -?Dati aggiuntivi per la 14ª riga (esenzione Iva)?
283100130205         if  DB0ift <> *zero  and
283200130205             DB0iva =  *zero  and
283300130205            (DB0cei <> *blank  or  DB0dce <> *blank);
283400130213           w_Row += 1;
283500130213           wx = w_Row;
283600130205           %occur(LdV) = wx;
283700130205           LdvPrg = 140;
283800130205           LdvTxt = ds_140;
283900130205         endif;
284000130205         // -?Dati aggiuntivi per la 18ª riga (n° fatt. e fil. Iva)?
284100130205         if  DB0nft <> *zero;
284200130213           w_Row += 1;
284300130213           wx = w_Row;
284400130205           %occur(LdV) = wx;
284500130205           LdvPrg = 180;
284600130205           LdvTxt = ds_180;
284700130205         endif;
284800110201
284900110201         // -?Codice a barre: ANNO+LNP+NRS+NSP+CKD:?
285000110201         //  ?richiama il pgm TRUL28R per ricavarne il check digit?
285100110201         reset  TRUL28ds;
285200110201         I28cod = BarCodeDS;
285300110201         TRUL28R1 ( TRUL28ds );
285400110201
285500110201         // -?Impostazione campo "LastRow"?
285600110201         LastRow = 'Spedizione: ' + %subst( O28cod : 1 : 15 ) + '  ' +
285700110203                   'Distinta: ' + %subst( ILVprmDst : 1 : 3 ) + '-'
285800170707                                + %subst( ILVprmDst : 4 ) +
285900170707                   '  (' + dILVlibero.§ILVapl + ')';
286000080801
286100110203         // ______________________________
286200110203         //?Stampa di prova (SE richiesta)?
286300110203         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
286400110519         If  SDSprm > 11;
286500130716           exsr  sr_Print;
286600130716         EndIf;
286700080430
286800080430       ENDSR;
286900130716
287000130716       //--------------------------------------------------------------
287100130716       //?Stampa dei dati per l'immagine - SE RICHIAMATO PER PROVA     ?
287200130716       //--------------------------------------------------------------
287300130716       BEGSR  sr_Print;
287400130716
287500130716         // -?Apertura file di stampa?
287600130716         if not %open(QSYSPRT);
287700130716           open  QSYSPRT;
287800130716         endif;
287900130716
288000130716         // -?Stampa "testata"?
288100130716         except PRTtxt;
288200130716
288300130716         // -?Stampa dettaglio?
288400130716         for wx = 1 to w_Row;
288500130716           %occur(LdV) = wx;
288600130716           except PRTrow;
288700130716         endfor;
288800130716
288900130716         // -?Stampa "ultima riga"?
289000130716         except PRTend;
289100130716
289200130716         // -?Chiusura file di stampa?
289300130716         close QSYSPRT;
289400130716
289500130716       ENDSR;
289600081030
289700081030       //--------------------------------------------------------------
289800130716       //?Preparazione dati x spedizione "DPD"                         ?
289900081030       //--------------------------------------------------------------
290000130716       BEGSR  sr_LdVbaseDPD;
290100081030
290200120524         // -?Impostazione del modulo DPD:?
290300131203         //// ·?Modulo DPD con logo Bartolini?
290400131203         //TemplateName = 'ldvbase02 ';
290500131203         //// ·?Modulo DPD con logo Bartolini/BRT?
290600131203         //TemplateName = 'ldvbase12 ';
290700120524         // ·?Modulo DPD con logo BRT?
290800120524         TemplateName = 'ldvbase22 ';
290900081030
291000101221         // -?Impostazione data?
291100081030         %occur(LdV) = 19;
291200081030         %subst(LdVTxt : 94) = 'Date ' + PdspA;
291300081030
291400101221         // -?Impostazione numero colli?
291500081030         %occur(LdV) = 20;
291600081030         %subst(LdVTxt : 94) = 'Parcel N.' + PnclA;
291700081030
291800101221         // -?Impostazione numero parcel?
291900081030         %occur(LdV) = 21;
292000171117         %subst(LdVTxt : 94) = PNDipn;
292100081111
292200131015         // -?"Decodifica" del barcode?
292300081111         %occur(LdV) = 22;
292400081111         %subst(LdVTxt : 94) = *blank;
292500171117         if  PNDipn <> *blank;
292600101221           // -?controllo se vecchio o nuovo barcode?
292700171117           if  %subst(PNDipn : 14 : 1) = *blank;
292800081111             // - - vecchio
292900171117             %subst(LdVTxt : 94) = '01 ' + %subst(PNDipn : 1 : 3)
293000171117                                 + ' '   + %subst(PNDipn : 4 : 8);
293100081111           else;
293200081111             // - - nuovo
293300171117             %subst(LdVTxT : 94) = '01 ' + %subst(PNDipn : 1 :  4)
293400171117                                 + ' '   + %subst(PNDipn : 5 : 10);
293500081111           endif;
293600081111         endif;
293700081111
293800130716         // -?Impostazione costanti  (LB ha detto di NON metterle)?
293900081111         //%occur(LdV) = 23;
294000081111         //%subst(LdVTxt : 94) = *blank;
294100081111         //%occur(LdV) = 24;
294200081111         //%subst(LdVTxt : 94) = 'Controllare la corrispon-';
294300081111         //%occur(LdV) = 25;
294400081111         //%subst(LdVTxt : 94) = 'denza del destinatario con';
294500081111         //%occur(LdV) = 26;
294600081111         //%subst(LdVTxt : 94) = 'l''indirizzo dell''etichetta';
294700081111         //%occur(LdV) = 27;
294800081111         //%subst(LdVTxt : 94) = 'DPD.';
294900081030
295000081030       ENDSR;
295100081030
295200081030       //--------------------------------------------------------------
295300130716       //?Preparazione dati x spedizione "FedEx"                       ?
295400081030       //--------------------------------------------------------------
295500130716       BEGSR  sr_LdVbaseFED;
295600081030
295700120524         // -?Impostazione del modulo FedEx:?
295800131203         //// ·?Modulo FedEx con logo Bartolini?
295900131203         //TemplateName = 'ldvbase03 ';
296000131203         //// ·?Modulo FedEx con logo Bartolini/BRT?
296100131203         //TemplateName = 'ldvbase13 ';
296200120524         // ·?Modulo FedEx con logo BRT?
296300120524         TemplateName = 'ldvbase23 ';
296400110517
296500081103         w_Row  = c_Row_B;
296600081030         Wtrc = 'C';
296700081030
296800101221         // -?Preparazione elenco segnacolli FedEx?
296900101213         if  NOT %open(FNART01L);
297000101213           open(e)  FNART01L;
297100101213           if  %error;
297200101213             exsr  sr_ErrOpenFile;
297300101213           endif;
297400101213         endif;
297500101213         if  NOT %open(FIARS01L);
297600101213           open(e)  FIARS01L;
297700101213           if  %error;
297800101213             exsr  sr_ErrOpenFile;
297900101213           endif;
298000101213         endif;
298100081030
298200101221         // -?Impostazione numeratore fino alla 39ª riga?
298300101213         FOR  wx = wx  TO (w_Row - c_Row_Fed);
298400081030
298500081030           %occur(LdV) = wx;
298600081030           LdvPrg      = wx;
298700081030
298800101213         ENDFOR;
298900081030
299000101221         // -?40ª riga & successive?
299100081030         clear  xx;
299200081030         clear  Wnsc;
299300101221         FOR  wx = (w_Row - c_Row_Fed + 1)  TO w_Row;
299400081030
299500081030           %occur(LdV) = wx;
299600081030           LdvPrg      = wx;
299700081030
299800081030           // - Caricamento AWB FedEx
299900081103           clear  wAWB;
300000101221           // - -?leggo segnacolli per bolle in arrivo?
300100081030           exsr  CarAWB_Arx;
300200081030           if  wAWB = *blank;
300300081030             leave;
300400081030           endif;
300500081030
300600081030           xx    += 1;
300700081103           Pptn   = wAWB;
300800081030           Psts   = %editc( xx : 'Z' );
300900081030           LdvTxt = ds_Fed;
301000081030
301100101221         ENDFOR;
301200081030
301300081030       ENDSR;
301400081030
301500081030       //--------------------------------------------------------------
301600130716       //?Riempimento del box "Promemoria per l'autotrasportore"       ?
301700081030       //--------------------------------------------------------------
301800130716       BEGSR  sr_LdVbase01;
301900081030
302000120524         // -?Impostazione del modulo con Box aggiuntivo (14 rec. in +)?
302100131203         //// ·?Modulo Box aggiuntivo con logo Bartolini?
302200131203         //TemplateName = 'ldvbase01 ';
302300131203         //// ·?Modulo Box aggiuntivo con logo Bartolini/BRT?
302400131203         //TemplateName = 'ldvbase11 ';
302500120524         // ·?Modulo Box aggiuntivo con logo BRT?
302600120524         TemplateName = 'ldvbase21 ';
302700110517
302800081103         w_Row  = c_Row_B;
302900081030
303000101221         // -?Intestazione box "Promemoria per l'autotrasportatore"?
303100131206         //  ?(NON LA SI FA PIÙ)?
303200131206         //exsr  sr_IntestBox;
303300131206
303400131206
303500131206         // -?Posizionamento sulla 36ª riga?
303600131206         //  ?(Il ciclo "FOR" precedente dovrebbe aver già posizionato?
303700131206         //  ? sulla 37ª riga)?
303800131206         wx = 36;
303900131206
304000131206         // -?Se reperito il 1° PASSAGGIO (c/o destinatario):?
304100131206         IF  $1o_Passaggio;
304200131206
304300131206           test(de) EVBdev;
304400131206           if %error;
304500131206             clear w0080;
304600131206           else;
304700131206             Data_Eur = %date( %char(EVBdev) : *iso0 );
304800131206             w0080    = %dec(Data_Eur);
304900131206           endif;
305000131206
305100131206           // -?37ª riga?
305200131206           wx += 1;
305300131206           %occur(LdV) = wx;
305400131206           LdvPrg      = wx;
305500131206           Ppma    = 'Primo passaggio del '
305600131206                   + %editc( w0080 : 'Y' )
305700131206                   + ' effettuato alle '
305800131206                   + %editw( EVBhev: '  :  ' );
305900131206           LdvTxt  = ds_37;
306000131206
306100131206           // -?38ª riga: VUOTA (separatrice)?
306200131206           wx += 1;
306300131206           %occur(LdV) = wx;
306400131206           LdvPrg      = wx;
306500131206           clear  LdvTxt;
306600131206
306700131206         ENDIF;
306800131206
306900081030
307000131206         // -?Se reperite CONSEGNE PARZIALI PRECEDENTI:?
307100131206         IF  $ConsParz = *on;
307200131206
307300131206           // -?Impostazione segnalazione in output?
307400131206           xx = 1;
307500131206           DoW  (xx <= %elem($CP)   and   $CP(xx) < *all'9');
307600131206
307700131206             Elem_CP = $CP(xx);
307800131206             test(de) WCPdcm;
307900131206             if %error;
308000131206               clear w0080;
308100131206             else;
308200131206               Data_Eur = %date( %char(WCPdcm) : *iso0 );
308300131206               w0080    = %dec(Data_Eur);
308400131206             endif;
308500131206
308600131206             // -?37ª o 39ª riga & successive?
308700131206             wx += 1;
308800131206             %occur(LdV) = wx;
308900131206             LdvPrg      = wx;
309000131206             Ppma    = 'Consegnati colli '
309100131206                     + %editc(WCPncl:'Z')
309200131206                     + ' in data '
309300140304                     + %editc(w0080:'Y');
309400140304             if  WCPocm > *zero;
309500140304               Ppma    = %trimr( Ppma )
309600140304                       + ' alle ore '
309700140304                       + %editw( WCPocm : '0 :  ' );
309800140304             endif;
309900140304             if  WCPfir <> *blank;
310000140304               Ppma    = %trimr( Ppma )
310100140303                     + ' - Firmatario: '
310200140303                     + WCPfir;
310300140304             endif;
310400131206             LdvTxt  = ds_37;
310500131206
310600131206             xx += 1;
310700131206
310800131206           EndDo;
310900131206
311000131206         ENDIF;
311100081030
311200081030       ENDSR;
311300080801
311400080801       //--------------------------------------------------------------
311500130716       //?Intestazione del box "Promemoria per l'autotrasportore"      ?
311600080801       //--------------------------------------------------------------
311700131206       //BEGSR  sr_IntestBox;
311800131206       //
311900131206       //  // 37ª riga
312000131206       //  //wx += 1;         (già incrementato dal ciclo "FOR" precedente)?
312100131206       //  %occur(LdV) = wx;
312200131206       //  LdvPrg      = wx;
312300131206       //  Ppma    = '                             '
312400131206       //          + c_PMxA;
312500131206       //  LdvTxt  = ds_37;
312600131206       //
312700131206       //  // -?38ª riga?
312800131206       //  wx += 1;
312900131206       //  %occur(LdV) = wx;
313000131206       //  LdvPrg      = wx;
313100131206       //  Ppma    = *all'=';
313200131206       //  LdvTxt  = ds_37;
313300131206       //
313400131206       //ENDSR;
313500101221
313600080430      /end-free
313700081030      *
313800081030      *---------------------------------------------------------------*
313900081030      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
314000081030      *---------------------------------------------------------------*
314100081030     c     CarAWB_Arx    BEGSR
314200130215      *
314300130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
314400130215      *   comunque una bolla mamma
314500130215     c                   exsr      sr_Mamma_FedEx
314600081030      *
314700081030      * Posizionamento
314800130215if  2c                   if        $Mamma = *off   and
314900130215     c                             $Mamma_FedEx = *off
315000081030     c     KLDVnsc       setgt     FNART000
315100081030x   2c                   else
315200081030     c     KLBLnsc       setgt     FNART000
315300081030e   2c                   endif
315400081030      *
315500081030      * Ciclo di lettura
315600081030do  1c                   DO        *hival
315700081030      *
315800130215if  2c                   if        $Mamma = *off   and
315900130215     c                             $Mamma_FedEx = *off
316000081030     c     KLDV          reade     FNART000
316100081030x   2c                   else
316200081030     c     KLBL          reade     FNART000
316300081030e   2c                   endif
316400081030if  2c                   if        %eof(FNART01L)
316500081030     c                   leave
316600081030e   2c                   endif
316700081030      * - per ogni segnacollo cerco l'AWB
316800081030     c     K05ARS01A     chain     FIARS000
316900081030if  2c                   if            %found(FIARS01L)
317000081030     c                             and %subst(ARSnot:1:1) >= *zeros
317100081030     c                   eval      wAWB = %subst(ARSnot:1:12)
317200081030     c                   eval      wNSC = ARSnsc
317300081030     c                   leave
317400081030e   2c                   endif
317500081030      *
317600081030e   1c                   ENDDO
317700081030      *
317800081030     c                   ENDSR
317900130215      *
318000130215      /free
318100130215
318200130215       //--------------------------------------------------------------
318300130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
318400130215       //--------------------------------------------------------------
318500130215       BEGSR  sr_Mamma_FedEx;
318600130215
318700130924         //$Mamma_FedEx = *off;         ?(già così)?
318800130215
318900130924         If  Not $Mamma;
319000130215
319100130215           // -?Verifica se la spedizione ha comunque mamma?
319200130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
319300130215           if  not %open(FNLBL01L);
319400130215             open  FNLBL01L;
319500130215           endif;
319600130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
319700130215
319800130215           $Mamma_FedEx = %found(FNLBL01L);
319900130215
320000130215         EndIf;
320100130215
320200130215       ENDSR;
320300130215
320400130215      /end-free
320500131008      *
320600131008      *---------------------------------------------------------------*
320700131008      *?Prepara elenco SegnaColli Non Sequenziali (dett.) - MAX 2 -  ?*
320800131008      *?  da FNART (arrivo)                                          ?*
320900131008      *---------------------------------------------------------------*
321000131008     c     ListaScNSd    BEGSR
321100131008      *
321200131008      * SE ci sono solo 2 segnacolli non sequenziali:
321300131008      *   staranno al posto del range - basta non indicare
321400131008      *   "NON SEQUENZIALI"
321500131008      *
321600131008if  1c                   if        not %open(FNART01L)
321700131008     c                   open(e)   FNART01L
321800131008      /free
321900131008           if  %error;
322000131008             exsr  sr_ErrOpenFile;
322100131008           endif;
322200131008      /end-free
322300131008e   1c                   endif
322400131008      *
322500131008      * memorizzo l'ultimo numero di segnacollo
322600131008if  1c                   if        $Mamma = *off
322700131008     c     KLDV          setgt     FNART000
322800131008     c     KLDV          readpe    FNART000
322900131008x   1c                   else
323000131008     c     KLBL          setgt     FNART000
323100131008     c     KLBL          readpe    FNART000
323200131008e   1c                   endif
323300131008if  1c                   if        NOT %eof(FNART01L)
323400131008     c                   eval      %subst(Pnsx : 8) = ' , '
323500131008     c                                              + %editc(ARTnsc : 'Z')
323600131008e   1c                   endif
323700131008      *
323800131008      * memorizzo il primo numero di segnacollo
323900131008if  1c                   if        $Mamma = *off
324000131008     c     KLDV          setll     FNART000
324100131008     c     KLDV          reade     FNART000
324200131008x   1c                   else
324300131008     c     KLBL          setll     FNART000
324400131008     c     KLBL          reade     FNART000
324500131008e   1c                   endif
324600131008if  1c                   if        NOT %eof(FNART01L)
324700131008     c                   eval      %subst(Pnsx : 1 : 7) =
324800131008     c                                 %editc(ARTnsc : 'Z')
324900131008e   1c                   endif
325000131008      *
325100131008     c                   ENDSR
325200030515      *
325300030515      *---------------------------------------------------------------*
325400050217      *?Carico LDV da FNARB00F                                       ?*
325500030515      *---------------------------------------------------------------*
325600110208     c     sr_CarARB     BEGSR
325700030515      *
325800110202      /free
325900110202
326000110202         //____________________________________________________________
326100110202         //? RISULTATO è impostato di default a "OK" => creo immagine ?
326200110202         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
326300110202
326400110202         // »? STEP 1 ?«
326500131203         //  ?Verifica se presenti forzature per la distinta           ?
326600131203         //  ?(vedi file FIIRD00F)                                     ?
326700131203
326800131203         //?-?Verifica se distinta in FIIRD10F                           ?
326900131203         //  ?e se elaborabile con (IRDTPELAB = *blank) o senza controlli?
327000130716         chain  %kds(k01fiird11)  FIIRD000;
327100130313
327200130716         If  %found(FIIRD11L)  and  IRDtpElab = *blank;
327300131203           // -?SE rileva errori =>?
327400131203           // -?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
327500130716           exsr  sr_STEP_1_CtrlTASFLO;
327600130716         EndIf;
327700131203
327800131203         // »? STEP 2 ?«
327900131203         //  ?Verifica se spedizione elaborabile                       ?
328000131203         //  ?(controlli su distinta e spedizione nel file FIILV00F)   ?
328100131203         //  ?- SE distinta SENZA forzature in FIIRD00F -              ?
328200110202
328300110204         // -?Reperimento dati distinta (FIDST)?
328400110228         clear  dDstflr;
328500110202         k_DSTnpg = 4;
328600110202         k_DSTnfv = ds_Distinta.p_Ndc;
328700110202         k_DSTfgs = ds_Distinta.p_Fgs;
328800110202         chain  %kds(k03fidst01)  FIDST000;
328900110228         if  %found(FIDST01L);
329000110228           dDSTflr = DSTflr;
329100110228         endif;
329200110202
329300110202         // -?Controllo distinta:?
329400130920         //?·?(SE Distinta NON trovata in FIIRD10F per recupero immagini?
329500130924         //  ?  & NON è una spedizione FORZATA PER TEST)                ?
329600131016         //  ?SE distinta NON trovata in FIDST01L: registra LOG e prosegue?
329700131016         //  ?SE distinta NON chiusa:?
329800131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
329900130716         If  Not %found(FIIRD11L)  and
330000130716             $Test = *blank;
330100131203           exsr  sr_STEP_2_CtrlDISTINTA;
330200130716         EndIf;
330300110202
330400110202
330500110202         // -?Reperimento dati Immagine LdV (FIILV)?
330600110208         clear  ILVcol;
330700110309         clear  wCountRec01;
330800110202         chain(e)  %kds(k06fiilv01)  FIILV000;
330900130313
331000130716         //?·?DISTINTA in FIIRD10F => recupero immagine (senza ctrl)?
331100131016         //?·?DISTINTA NON in FIIRD10F => controlli da eseguire     ?
331200131016         //  ?SE trovati errori:?
331300131016         //  ?"risponde" ed eventualmente esce dalla s.p. (direttamente dalla subr.)?
331400130716         If  Not  %found(FIIRD11L);
331500131203           exsr  sr_STEP_2_CtrlFIILV;
331600130716         EndIf;
331700130313
331800131203         // »? STEP 3 ?«
331900131203         //  ?Controllo spedizione                                     ?
332000131203         //  ?(da eseguire comunque)                                   ?
332100110202
332200131203         // -?Reperimento dati Spedizione (FNARB)?
332300110207         chain  KLDV  FNARB000;
332400101220
332500110202         // -?Spedizione non trovata?
332600101221         if  NOT %found(FNARB01L);
332700130924           // ·?Spedizione non trovata => ERRORE: non rielaborare immagine?
332800131203           //  ?"risponde" ed esce dalla s.p.?
332900110204           Risultato = 'XX';
333000130924           //RtnOpCode = 'FNARB_MANC';
333100130924           //RtnEsito  = -1;
333200130924           // -?Registrazione log in FIILV, differenziando l'errore per?
333300130924           //  ?iter normale (25) o iter con forzatura (52)?
333400130924           if  Not  %found(FIIRD11L);
333500130313             wCod_Log = '25';
333600130716           else;
333700130716             wCod_Log = '52';
333800130716           endif;
333900110208           exsr  sr_WrtFIILV;
334000101221           // -?Uscita dalla s.p.?
334100110204           exsr  sr_End;
334200101221         endif;
334300101221
334400101221
334500110202         // -?Registrazione record per spedizione "OK" (root "buono")?
334600110218         If  wCod_Log = *blank;
334700130206           RootFolder = §ILVimgOk;
334800110203           wCod_Log = '01';
334900110202           exsr  sr_WrtFIILV;
335000110218         EndIf;
335100090121
335200080523      /end-free
335300070514      *
335400070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
335500070514      *   il reperimento dell'eventuale bolla mamma...
335600070514     c                   eval      wB0ntc =  ARBntc
335700120524     c                   eval      wB0dcp =  ARBdcp
335800120524     c                   eval      wB0tmc =  ARBtmc
335900120524     c                   move      ARBpdc        Wpdc4a
336000080523     c                   eval      wB0dcm =  ARBdcm
336100121018     c                   eval      wB0piv =  ARBcpi
336200030515      *
336300030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
336400030515     c                   exsr      sr_DS_ARB
336500030515      *
336600070308      * Decodifico filiale di Arrivo
336700131203     c                   exsr      sr_DecLNA
336800030515      *
336900030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
337000030515      *    "generiche" (3A, TB, 5E)
337100030515     c                   exsr      sr_DS_TAB
337200030515      *
337300030515      * Ultimi dati mancanti:
337400030515      * - Cod. cliente mittente
337500030515if  1c                   If        ARBccm <> *zeros
337600030515     c                   movel     ARBccm        DB0ccm
337700030515x   1c                   Else
337800030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
337900030515     c                   movel     ARBksc        DB0ccm
338000030515e   2c                   endif
338100030515e   1c                   EndIf
338200030515      * - Cod. cliente destinatario
338300030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
338400030515     c                   movel     ARBksc        DB0ccd
338500030515e   1c                   endif
338600030515      * - Se c'è 2ª bolla imposto LNA9999
338700030515if  1c                   if        DB0tb2 <> *blanks
338800030515     c                   eval      DB0ccd = *all'9'
338900030515     c                   movel     ARBlna        DB0ccd
339000030515e   1c                   endif
339100120524      * - Reperimento Data spedizione
339200120524if  1c                   if        DB0mgs <> *zeros
339300120524     c                   z-add     DB0mgs        Wdsp
339400120524     c                   movel     DB0aas        Wdsp
339500120524x   1c                   else
339600120524      *   prendo la data del giorno se non c'è la data spedizione
339700120524     c                   time                    w0140
339800120524     c                   move      w0140         w0080
339900120524     c     *eur          movel     w0080         Data_EUR
340000120524     c     *iso          movel     Data_EUR      Wdsp
340100120524e   1c                   endif
340200030515      *
340300061030      * 3ª impostaz. campi della DS: importi da calcolare
340400061031     c                   exsr      sr_DS_IMPO
340500030515      *
340600080505     c                   ENDSR
340700080724      /free
340800130716
340900130716       //--------------------------------------------------------------
341000130716       //?»  STEP 1  «                                                 ?
341100130716       //?Distinta in FIIRD10F e senza forzature (IRDTPLAB = *blank):  ?
341200130716       //?verifica se immagine già generata (DTASFLO.§FLOIML <> *blank)?
341300130716       //--------------------------------------------------------------
341400130716       BEGSR  sr_STEP_1_CtrlTASFLO;
341500130716
341600130716         // -?Reperimento spedizione da TITAS per verificarne lo stato?
341700131016         //  ?della relativa immagine.  Se già presente immagine:?
341800131016         //  ?"risponde" con codice errore 51 e "non elaborare"?
341900131016         //  ?ed esce dalla s.p.?
342000130716         if  %subst( wSysName : 1 : 6 ) = 'SETRAS';
342100130716           wLibFileTITAS = 'GAITRAGRU/TITAS30C';
342200130716         else;
342300130716           wLibFileTITAS = 'GAITRAGRPS/TITAS30C';
342400130716         endif;
342500130716         if  NOT  %open(TITAS30C);
342600130716           open(e)  TITAS30C;
342700130716           if  %error;
342800130716             exsr  sr_ErrOpenFile;
342900130716           endif;
343000130716         endif;
343100130716
343200130716         chain  %kds(k05titas30 : 4)  TITAS30C;
343300130716
343400130716         If  %found(TITAS30C)            and
343500130716             TASdcm > *zero              and
343600130716             TASndc = ds_Distinta.p_Ndc  and
343700130716             %int( TASpdc / 10000 ) = ds_Distinta.p_Fgs;
343800130716
343900130716           dTASflo = TASflo;
344000130716
344100130716           if  dTASflo.§FLOiml <> *blank;
344200130716             Risultato = 'XX';
344300130924             //RtnOpCode = 'GIÀ_IMMAG ';
344400130924             //RtnEsito  = -1;
344500130716             // -?Registrazione log in FIILV?
344600130716             wCod_Log = '51';
344700130716             exsr  sr_WrtFIILV;
344800130716             // -?Uscita dalla s.p.?
344900130716             exsr  sr_End;
345000130716           endif;
345100130716
345200130716         EndIf;
345300130716
345400130716       ENDSR;
345500130716
345600130716       //--------------------------------------------------------------
345700131203       //?»  STEP 2  «                                                 ?
345800131203       //?Distinta NON in FIIRD10F:?controlli sulla distinta.          ?
345900130716       //--------------------------------------------------------------
346000131203       BEGSR  sr_STEP_2_CtrlDISTINTA;
346100130716
346200130716         Select;
346300130716
346400130716           // ·?DISTINTA non trovata => ANOMALIA?
346500130716           When  NOT  %found(FIDST01L)  or
346600130716                 DSTatb <> *blank;
346700130924             // -?Registrazione log in FIILV - se non già fatto?
346800130716             if  wCod_Log = *blank;
346900130716               wCod_Log = '22';
347000130716               exsr  sr_WrtFIILV;
347100130716             endif;
347200130716
347300131016           // ·?DISTINTA non chiusa => ripetere la richiesta?
347400130716           When  DSTfcf = *blank;
347500130716             Risultato  = 'NO';
347600130924             //RtnOpCode  = 'DIST_APERT';
347700130716             //RtnEsito   = -1;
347800130716             // -?Uscita dalla s.p.?
347900130716             exsr  sr_End;
348000130716
348100130716         EndSl;
348200130716
348300130716       ENDSR;
348400130716
348500130716       //--------------------------------------------------------------
348600131203       //?»  STEP 2  «                                                 ?
348700131203       //?Distinta NON in FIIRD10F:?controlli sul/i record in FIILV00F.?
348800130716       //--------------------------------------------------------------
348900131203       BEGSR  sr_STEP_2_CtrlFIILV;
349000130716
349100130716         Select;
349200130716
349300130716           // ·?Rec. IMMAGINE LdV allocato => ripetere la richiesta?
349400130716           When  %error;
349500130716             Risultato  = 'NO';
349600130924             //RtnOpCode  = 'FIILV_ALLC';
349700130716             //RtnEsito   = -1;
349800130716             // -?Uscita dalla s.p.?
349900130716             exsr  sr_End;
350000130716
350100130716           // ·?Rec. IMMAGINE LdV non trovato?
350200130716           When  NOT  %found(FIILV01L);
350300130716             select;
350400130716               // ·?DISTINTA senza scarico a PDA delle spedizioni?
350500130716               //  ?=> cancellare immagine (senza LOG)?
350600130716               when  DSTpda <> 'C'   and   DSTpda <> 'E';
350700130716                 Risultato  = 'XX';
350800130924                 //RtnOpCode  = 'DIST_NOPDA';
350900130716                 //RtnEsito   = -1;
351000130716                 // -?Uscita dalla s.p.?
351100130716                 exsr  sr_End;
351200130716               // ·?DISTINTA in test consegne?
351300130716               //  ?=> cancellare immagine (senza LOG)?
351400130716               when  §DSTtstPda = 'C'   or   §DSTtstPda = 'E';
351500130716                 Risultato  = 'XX';
351600130924                 //RtnOpCode  = 'DIST_TEST ';
351700130716                 //RtnEsito   = -1;
351800130716                 // -?Uscita dalla s.p.?
351900130716                 exsr  sr_End;
352000130716               // ·?Altrimenti: ANOMALIA?
352100130924               //  ?Registrazione log in FIILV - se non già fatto?
352200130716               other;
352300130716                 if  wCod_Log = *blank;
352400130716                   wCod_Log = '23';
352500130716                   exsr  sr_WrtFIILV;
352600130716                 endif;
352700130716             endsl;
352800130716
352900130716           // ·?Rec. IMMAGINE LdV già elaborato?
353000130716           //  ?=> ricerca record NON elaborato (ILVCOL = "00")?
353100130716           When  ILVcol <> '00';
353200130716             exsr  sr_CercaLog00;
353300130716             // ·?Rec. IMMAGINE LdV non trovato (né "00" né "01")?
353400130924             //  ?Registrazione log in FIILV - se non già fatto?
353500130716             if  ILVcol <> '00'  and  wCountRec01 = *zero
353600130716                                 and  wCod_Log    = *blank;
353700130716               wCod_Log = '23';
353800130716               exsr  sr_WrtFIILV;
353900130716             endif;
354000130716
354100130716         EndSl;
354200130716
354300130716
354400130716         // -?Verifica SE trovato record da elaborare per la sped/dist?
354500130716         $FIILV = (ILVcol = '00');
354600130716
354700130716         // -?Impostazione struttura dati del campo ILVLIBERO?
354800130716         dILVlibero = ILVlibero;
354900130716
355000130716
355100130716         Select;
355200130716
355300130716           // -?Spedizione non consegnata in distinta?
355400130716           When  ILVnoCDst = 'S';
355500130924             // -?Registrazione log in FIILV - se non già fatto?
355600130716             if  wCod_Log = *blank;
355700130716               wCod_Log = '08';
355800130716               exsr  sr_WrtFIILV;
355900130716             endif;
356000130716
356100130716           // -?LdV da scannerizzare?
356200130716           When  ILVscan = *blank;
356300130924             // -?Registrazione log in FIILV - se non già fatto?
356400130716             if  wCod_Log = *blank;
356500130716               wCod_Log = '09';
356600130716               exsr  sr_WrtFIILV;
356700130716             endif;
356800130716
356900130716         EndSl;
357000130716
357100130716       ENDSR;
357200130716
357300130716       //--------------------------------------------------------------
357400130716       //?Ricerca record IMMAGINE LDV NON elaborato (ILVCOL = "00")    ?
357500130716       //--------------------------------------------------------------
357600130716       BEGSR  sr_CercaLog00;
357700130716
357800130716         //?N.B.?Il 1° rec. per spedizione/distinta è già stato letto?
357900130716
358000130716         DoU  ILVcol = '00';
358100130716
358200130716           // ·?Rec. Immagine LdV già elaborato ("01")?
358300130716           if  ILVcol = '01';
358400130716             wCountRec01 += 1;
358500130716           endif;
358600130716
358700130716           reade(e)  %kds(k06fiilv01)  FIILV000;
358800130716
358900130716           select;
359000130716
359100130716             // ·?Rec. Immagine LdV allocato => ripetere la richiesta?
359200130716             when  %error;
359300130716               Risultato  = 'NO';
359400130924               //RtnOpCode  = 'FIILV_ALLC';
359500130716               //RtnEsito   = -1;
359600130716               // -?Uscita dalla s.p.?
359700130716               exsr  sr_End;
359800130716
359900130716             // ·?Solo rec. Immagine LdV già elaborati?
360000130716             //  ?=> scrittura nuovo?
360100130716             when  %eof(FIILV01L);
360200130716               clear  ILVcol;
360300130716               leave;
360400130716
360500130716             // ·?Già reperiti 100 rec. per le stesse sped./dist.?
360600130716             //  ?(questo è in realtà il 101°)?
360700131016             //  ?=> NON ripetere la richiesta (senza LOG)?
360800130716             when  wCountRec01 >= 100;
360900130716               Risultato  = 'XX';
361000130924               //RtnOpCode  = 'SPEDGIÀ100';
361100130716               //RtnEsito   = -1;
361200130716               // -?Invio e-mail?
361300130716               exsr  sr_SendMAIL;
361400130716               // -?Uscita dalla s.p.?
361500130716               exsr  sr_End;
361600130716
361700130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
361800130716             //  ?creazione immagine sul PDA successive all'attuale?
361900131016             //  ?=> NON ripetere la richiesta (senza LOG)?
362000130716             //when  DataOraCrea <= ILVprmTimP;
362100130716             when  DataOraCrea < ILVprmTimP;
362200130716               Risultato  = 'XX';
362300130924               //RtnOpCode  = 'SPED_GIÀEL';
362400130716               //RtnEsito   = -1;
362500130716               // -?Uscita dalla s.p.?
362600130716               exsr  sr_End;
362700130716
362800130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
362900130716             //  ?creazione immagine sul PDA identica all'attuale?
363000130716             //  ?=> riprovare dopo 10 minuti (senza LOG)?
363100130716             when  DataOraCrea = ILVprmTimP  and
363200130716                   ILVtimA > %int( %subst( %char( %dec(
363300130716                                   %timestamp() - %minutes(10) ) )
363400130716                                           : 1 : 14 ) );
363500130716               Risultato  = 'NO';
363600130924               //RtnOpCode  = '..TRA10MIN';
363700130716               //RtnEsito   = -1;
363800130716               // -?Uscita dalla s.p.?
363900130716               exsr  sr_End;
364000130716
364100130716           endsl;
364200130716
364300130716         EndDo;
364400130716
364500130716       ENDSR;
364600101221
364700080724       //--------------------------------------------------------------
364800130716       //?Registrazione record nel work-file FIILV00F                  ?
364900080724       //--------------------------------------------------------------
365000110207       BEGSR  sr_WrtFIILV;
365100110202
365200110202         // -?Segnalazione anomalia rilevata per la spedizione?
365300110204         if  Not  $FIILV;
365400110202           clear  FIILV000;
365500110202           ILVfgs = ds_Distinta.p_Fgs;
365600110202           ILVndc = ds_Distinta.p_Ndc;
365700110202           ILVaas = DB0aas;
365800110202           ILVlnp = DB0lnp;
365900110202           ILVnrs = DB0nrs;
366000110202           ILVnsp = DB0nsp;
366100110202         endif;
366200110202
366300110207         ILVcol     = wCod_Log;
366400110202         xx = %lookup( wCod_Log : $CoL );
366500110202         if  xx > *zero;
366600110202           ILVnot   = $DeL(xx);
366700110202         endif;
366800110202         ILVprmSpe  = Spedizione;
366900110202         ILVprmDst  = Distinta;
367000110202         ILVprmTimp = DataOraCrea;
367100110202         ILVrisult  = Risultato;
367200110202         ILVtimA    = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
367300170707
367400170707         dILVlibero.§ILVapl = 'G';
367500170707
367600170707         ILVlibero = dILVlibero;
367700130411
367800130411
367900131203         // -? Spedizioni FORZATE PER TEST => NON scrive il rec.       ?
368000130411         IF  $Test <> *blank;
368100130411           leavesr;
368200130411         ENDIF;
368300130411
368400090210
368500110923
368600131203         // -?S.P. richiamata per stampa cartacea => NON scrive il rec.?
368700110923         If  SDSprm > 11;
368800110923           leavesr;
368900110923         EndIf;
369000110923
369100110923
369200110204         if  $FIILV;
369300110202           //_______________
369400110202           update  FIILV000;
369500110202           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
369600110202         else;
369700110204           //_______________
369800110204           write   FIILV000;
369900110204           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
370000110202         endif;
370100080724
370200080724       ENDSR;
370300130227
370400130227       //--------------------------------------------------------------
370500130716       //?Invio e-mail per avvisare degli oltre 100 rec. con cod. LOG  ?
370600130716       //?   "01" per la stessa spedizione in FIILV.                   ?
370700130227       //--------------------------------------------------------------
370800130227       BEGSR  sr_SendMAIL;
370900130227
371000130227         // -?Reperimento tab. "MRA"?
371100130227         reset  dMRAdan;
371200130227         reset  TIBS02ds;
371300130227         T02cod = 'MRA';
371400130227         T02ke1 = SDSpgm;
371500130227         T02ke2 = *blank;
371600130227         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
371700130227         if  T02err = *blank;
371800130227           dMRAdan = T02uni;
371900130227         endif;
372000130227
372100130227         // -?Override ai file di stampa?
372200130227         //  ?(per impostarvi i dati per l'invio via e-mail)?
372300130227         exsr sr_OvrPrtF;
372400130227
372500130227         // -?Stampa segnalazione?
372600130227         except  EmlTxt;
372700130227         P_Fgs2 = ds_Distinta.p_Fgs;
372800130227         P_Ndc2 = ds_Distinta.p_Ndc;
372900130227         except  EmlDet;
373000130227         except  EmlEnd;
373100130227
373200130227         // -?Cancellazione Override ai file di stampa?
373300130227         exsr sr_DeleteOvr;
373400130227
373500130227       ENDSR;
373600130227
373700130227       //--------------------------------------------------------------
373800130716       //?Override al file di stampa per impostarvi i dati per         ?
373900130716       //?  l'invio via e-mail.                                        ?
374000130227       //--------------------------------------------------------------
374100130227       BEGSR  sr_OvrPrtF;
374200130227
374300130227         reset  TRTCM1ds;
374400130227         if  §MRAdmitt <> *blank;
374500130227           §CM1mitt = %trim(§MRAdmitt);
374600130227         endif;
374700130227         §CM1dst  = %trim(§MRAddest);
374800130227         §CM1tips = §MRAdreg;
374900130227         //§CM1var  = '*OBJM*' + §MRAddes;
375000130227         §CM1idp  = §MRAdidpro;
375100130227
375200130227         // -?Override al "corpo e-mail"?
375300130227         if  §MRAdreg <> *blank;
375400130227           Qcmd = c_CmdOvrPrtF
375500130227                + ' outq(' + %trim(§MRAdoutqi) + ')'
375600130227                + ' usrdfndta(''' + TRTCM1ds + ''')';
375700130227         else;
375800130227           Qcmd = c_CmdOvrPrtF;
375900130227         endif;
376000130227
376100130227         exsr  sr_ExecCmd;
376200130227
376300130227         //if  Qusei <> *blank;
376400130227         //  ...;
376500130227         //endif;
376600130227
376700130227         // -?Apertura file di stampa?
376800130227         open  PrtEmail;
376900130227
377000130227       ENDSR;
377100130227
377200130227       //--------------------------------------------------------------
377300130227       //?Chiusura file di stampa e cancellazione override.            ?
377400130227       //--------------------------------------------------------------
377500130227       BEGSR  sr_DeleteOvr;
377600130227
377700130227         // -?Chiusura file di stampa?
377800130227         if  %open(PrtEMAIL);
377900130227           close  PrtEMAIL;
378000130227         endif;
378100130227
378200130227         // -?Cancellazione override al file di stampa?
378300130227         Qcmd = c_CmdDltOvr;
378400130227         exsr  sr_ExecCmd;
378500130227
378600130227       ENDSR;
378700130227
378800130227       //--------------------------------------------------------------
378900130227       //?Esecuzione del comando (già impostato).                      ?
379000130227       //--------------------------------------------------------------
379100130227       BEGSR  sr_ExecCmd;
379200130227
379300130227         clear Qcap0100;
379400130227         Qcabcsdh = *off;
379500130227         Qcapa    = *off;
379600130227         Qcacmdss = *off;
379700130227         Qcaerved = *allX'00';
379800130227
379900130227         clear Qusec;
380000130227         Qusbprv  = %size(Qusec);
380100130227
380200130227         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
380300130227                           %size(Qcap0100) : 'CPOP0100' : *omit :
380400130227                           0 : 0 : Qusec);
380500130227
380600130227         //if  Qusei <> *blank;
380700130227         //  ...;
380800130227         //endif;
380900130227
381000130227       ENDSR;
381100101221
381200080724      /end-free
381300030515      *
381400030515      *---------------------------------------------------------------*
381500050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
381600030515      *---------------------------------------------------------------*
381700030515     c     sr_DS_ARB     BEGSR
381800030515      *
381900030515     c                   eval      DB0cbo = ARBcbo
382000030515     c                   eval      DB0tsp = ARBtsp
382100030515     c                   eval      DB0mgs = ARBmgs
382200030515     c                   eval      DB0tc1 = ARBtc1
382300030515     c                   eval      DB0tc2 = ARBtc2
382400030515     c                   eval      DB0fns = ARBfns
382500030515     c                   eval      DB0rmn = ARBrmn
382600030515     c                   eval      DB0rma = ARBrma
382700030515     c                   eval      DB0ias = ARBias
382800030515     c                   eval      DB0vas = ARBvas
382900030515     c                   eval      DB0vmd = ARBvmd
383000030515     c                   eval      DB0vad = ARBvad
383100030515     c                   eval      DB0qft = ARBqft
383200030515     c                   eval      DB0lna = ARBlna
383300030515     c                   eval      DB0znc = ARBznc
383400030515     c                   eval      DB0rsm = ARBrsm
383500030515     c                   eval      DB0inm = ARBinm
383600030515     c                   eval      DB0cam = ARBcam
383700030515     c                   eval      DB0lom = ARBlom
383800030515     c                   eval      DB0prm = ARBprm
383900030515     c                   eval      DB0nzm = ARBnzm
384000030515     c                   eval      DB0fap = ARBfap
384100030515     c                   eval      DB0rmo = ARBrmo
384200030515     c                   eval      DB0rsd = ARBrsd
384300030515     c                   eval      DB0ind = ARBind
384400030515     c                   eval      DB0cad = ARBcad
384500030515     c                   eval      DB0lod = ARBlod
384600030515     c                   eval      DB0prd = ARBprd
384700030515     c                   eval      DB0nzd = ARBnzd
384800030515     c                   eval      DB0fin = ARBfin
384900030515     c                   eval      DB0gc1 = ARBgc1
385000030515     c                   eval      DB0gc2 = ARBgc2
385100030515     c                   eval      DB0nas = ARBnas
385200030515     c                   eval      DB0ncl = ARBncl
385300030515     c                   eval      DB0pkf = ARBpkf
385400030515     c                   eval      DB0fvf = ARBfvf
385500030515     c                   eval      DB0vlf = ARBvlf
385600030515     c                   eval      DB0fls = ARBfls
385700030515     c                   eval      DB0ncd = ARBncd
385800030515     c                   eval      DB0nca = ARBnca
385900030515     c                   eval      DB0xco = ARBxco
386000030515     c                   eval      DB0ffd = ARBffd
386100030515     c                   eval      DB0dcr = ARBdcr
386200030515     c                   eval      DB0hcr = ARBhcr
386300030515     c                   eval      DB0tcr = ARBtcr
386400030515     c                   eval      DB0fbr = ARBfbr
386500030515     c                   eval      DB0gga = ARBgga
386600030515     c                   eval      DB0gma = ARBgma
386700030515     c                   eval      DB0gva = ARBgva
386800030515     c                   move      ' '           DB0gva
386900030515     c                   clear                   DB0sop
387000030515     c                   eval      DB0ctr = ARBctr
387100030515     c                   clear                   DB0drt
387200030515     c                   clear                   DB0fpp
387300030515     c                   clear                   DB0nrt
387400030515     c                   clear                   DB0pdr
387500030515      *
387600030515      * Allega documento di trasporto
387700060524      * & Codice cliente da Assegnato
387800130924     c***?(già fatto:)?  clear                   dsBL4A
387900031029     c                   clear                   DB0bam
388000061107     c                   exsr      RepAR4_A
388100060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
388200031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
388300030515     c                   movel     'S'           DB0bam
388400060505     c                   endif
388500030515      *
388600030515      * Destinatario: vedo se esiste 2ª ragione sociale
388700061107     c                   clear                   DB0rd2
388800061107     c                   exsr      RepAR4_D
388900030515      *
389000030515      * Note
389100061107     c                   clear                   DB0nt1
389200061107     c                   clear                   DB0nt2
389300061107     c                   exsr      RepAR4_89
389400030515      *
389500030515     c                   ENDSR
389600030515      *
389700030515      *---------------------------------------------------------------*
389800050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
389900050217      *?   "generiche" (non da "1A")                                 ?*
390000030515      *---------------------------------------------------------------*
390100030515     c     sr_DS_TAB     BEGSR
390200030515      *
390300030515      * Carico le tabelle "generiche" (3A, TB, 5E)
390400030515     c                   exsr      CarTAB
390500030515      *
390600030515      * Codici bolla
390700030515     c                   eval      DB0tb1 = §3Atb1
390800030515     c                   eval      DB0tb2 = §3Atb2
390900030515     c                   eval      DB0dcb = §3Ades
391000030515      * Tipo servizio
391100030515if  1c                   if        DB0dts = *blanks
391200030515     c                   eval      DB0dts = §5Edes
391300030515     c                   eval      DB0fst = §5Efst
391400030515e   1c                   endif
391500030515      * Consegne particolari
391600030515if  1c                   if            DB0tc1 <> *blanks
391700030515     c                             and DB0dc1 =  *blanks
391800030515     c                   movel(p)  DB0tc1        TBLkey
391900030515     c                   exsr      Tab_1P
392000030515     c                   eval      DB0dc1 = §1Pdes
392100030515e   1c                   endif
392200030515if  1c                   if            DB0tc2 <> *blanks
392300030515     c                             and DB0dc2 =  *blanks
392400030515     c                   movel(p)  DB0tc2        TBLkey
392500030515     c                   exsr      Tab_1P
392600030515     c                   eval      DB0dc2 = §1Pdes
392700030515e   1c                   endif
392800030515      * Esenzione IVA
392900030515if  1c                   if            DB0cei <> *blanks
393000030515     c                             and DB0dce =  *blanks
393100030515     c                   eval      DB0dce =  §EIdes
393200030515e   1c                   endif
393300030515      *
393400030515     c                   ENDSR
393500060524      *
393600060524      *---------------------------------------------------------------*
393700060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
393800061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
393900060524      *---------------------------------------------------------------*
394000060524     c     RepAR4_A      BEGSR
394100060524      *
394200130924     c***?(già fatto:)?  clear                   dsBL4A
394300061107      *
394400060524     c                   eval      Wtrc = 'A'
394500060524     c     KLDVtrc       chain     FIAR4000
394600060524      *
394700060524if  1c                   if        %found(FIAR401L)
394800060524     c                   movel     AR4Not        dsBL4A
394900060524e   1c                   endif
395000060524      *
395100060524     c                   ENDSR
395200061107      *
395300061107      *---------------------------------------------------------------*
395400061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
395500061107      *?(2ª ragione sociale del destinatario)                        ?*
395600061107      *---------------------------------------------------------------*
395700061107     c     RepAR4_D      BEGSR
395800061107      *
395900130924     c***?NON esiste:?   clear                   dsBL4D
396000130924     c***?(già fatto:)?  clear                   DB0rd2
396100061107      *
396200061107     c                   eval      Wtrc = 'D'
396300061107     c     KLDVtrc       chain     FIAR4000
396400061107      *
396500061107if  1c                   if        %found(FIAR401L)
396600061107     c                   movel     AR4not        DB0rd2
396700061107e   1c                   endif
396800061107      *
396900061107     c                   ENDSR
397000171117      *//*
397100171117      *//*---------------------------------------------------------------*
397200171117      *//*?Reperimento dati da FIAR400F x trk "I"                       ?*
397300171117      *//*?(dati DPD)                                                   ?*
397400171117      *//*---------------------------------------------------------------*
397500171117     c*//  RepAR4_I      BEGSR
397600061030      *
397700130924     c***?(già fatto:)?  clear                   dsBL4I
397800061107      *
397900171117     c*//                eval      Wtrc = 'I'
398000171117     c*//  KLDVtrc       chain     FIAR4000
398100061030      *
398200171117if  1c*//                if        %found(FIAR401L)
398300171117     c*//                movel     AR4not        dsBL4I
398400171117e   1c*//                endif
398500061030      *
398600171117     c*//                ENDSR
398700071008      *
398800071015      *---------------------------------------------------------------*
398900071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
399000110204      *?(dati aggiuntivi per O.R.M. abbinato alla spedizione)        ?*
399100071015      *---------------------------------------------------------------*
399200071015     c     RepAR4_M      BEGSR
399300071015      *
399400130924     c***?(già fatto:)?  clear                   dsBL4M
399500071015      *
399600071015     c                   eval      Wtrc = 'M'
399700071015     c     KLDVtrc       chain     FIAR4000
399800071015      *
399900071015if  1c                   if        %found(FIAR401L)
400000071015     c                   movel     AR4Not        dsBL4M
400100071015e   1c                   endif
400200071015      *
400300071015     c                   ENDSR
400400121018      *
400500121018      *---------------------------------------------------------------*
400600121018      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
400700121018      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
400800121018      *---------------------------------------------------------------*
400900121018     c     RepAR4_Q      BEGSR
401000121018      *
401100130924     c***?(già fatto:)?  clear                   dTA4Q
401200130924     c***?(già fatto:)?  clear                   DB0cpi
401300121018      *
401400121018     c                   eval      Wtrc = 'Q'
401500121018     c     KLDVtrc       chain     FIAR4000
401600121018      *
401700121018if  1c                   if        %found(FIAR401L)
401800121018     c                   movel     AR4not        dTA4Q
401900121018e   1c                   endif
402000121018      *
402100121018     c                   ENDSR
402200080523      /free
402300080523
402400080523       //--------------------------------------------------------------
402500080523       //?Reperimento dati da FIAR400F x trk "1"                       ?
402600080523       //?(nominativo del destinatario)                                ?
402700080523       //--------------------------------------------------------------
402800080523       BEGSR  RepAR4_1;
402900080523
403000101222         //clear  dsBL41;        *** NON esiste ***?
403100080523
403200080523         Wtrc = '1';
403300080523         chain  KLDVtrc  FIAR4000;
403400080523
403500080523         if  NOT  %found(FIAR401L);
403600080523           clear  AR4not;
403700080523         endif;
403800080523
403900150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
404000081104           Pfedkcn = AR4not;
404100081104         else;
404200081104           Pkcn    = AR4not;
404300081104         endif;
404400080523
404500080523       ENDSR;
404600080523
404700080523      /end-free
404800061107      *
404900061107      *---------------------------------------------------------------*
405000061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
405100061107      *?(note)                                                       ?*
405200061107      *---------------------------------------------------------------*
405300061107     c     RepAR4_89     BEGSR
405400061107      *
405500130924     c***?NON esiste:?   clear                   dsBL48
405600130924     c***?NON usata:?    clear                   dsBL49
405700130924     c***?(già fatto:)?  clear                   DB0nt1
405800130924     c***?(già fatto:)?  clear                   DB0nt2
405900061107      *
406000061107     c                   eval      Wtrc = '8'
406100061107     c     KLDVtrc       chain     FIAR4000
406200061107if  1c                   if        %found(FIAR401L)
406300061107     c                   movel     AR4not        DB0nt1
406400061107e   1c                   endif
406500061107      *
406600061107     c                   eval      Wtrc = '9'
406700061107     c     KLDVtrc       chain     FIAR4000
406800061107if  1c                   if        %found(FIAR401L)
406900061107     c                   movel     AR4not        DB0nt2
407000061107e   1c                   endif
407100061107      *
407200061107     c                   ENDSR
407300171117      *
407400171117      *---------------------------------------------------------------*
407500171117      *?Reperimento dati da FIPND00F                                 ?*
407600171117      *?(dati DPD)                                                   ?*
407700171117      *---------------------------------------------------------------*
407800171117     c     RepFIPND      BEGSR
407900171117      *
408000171117if  1c                   if        NOT  %open(FIPND01L)
408100171117     c                   open      FIPND01L
408200171117e   1c                   endif
408300171117      *
408400171117     c     kLDV          chain     FIPND000
408500171117      *
408600171117if  1c                   if        NOT %found(FIPND01L)
408700171117     c                   clear                   PNDipn
408800171117e   1c                   endif
408900171117      *
409000171117     c                   ENDSR
409100030515      *
409200030515      *---------------------------------------------------------------*
409300050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
409400030515      *---------------------------------------------------------------*
409500030515     c     CarTAB        BEGSR
409600030515      *
409700030515      * Tab. "3A" - Codici Bolla
409800030515      * (NO se rimesso mittente)
409900030515     c                   clear                   ds3A
410000030515if  1c                   if        DB0ris <> 'M'
410100030515     c                   movel     '3A'          TBLcod
410200030515     c                   movel(p)  DB0cbo        TBLkey
410300030515     c     K03TAB        chain     TABEL
410400030515if  2c                   if        %found(TABEL00F)
410500030515     c                   movel     TBLuni        ds3A
410600030515e   2c                   endif
410700030515e   1c                   endif
410800070515     c                   eval      $Mamma =  (§3Abcm = 'S')
410900030515      *
411000030515      * Tab. "TB" - Tipi Bolla
411100030515      * (NO se rimesso mittente)
411200030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
411300030515      * se no non stampo i dati fattura
411400030515     c                   clear                   dsTB
411500030515if  1c                   if        DB0ris <> 'M'
411600030515     c                   movel     'TB'          TBLcod
411700121029if  2c                   if        §3Atb2 <> *blanks
411800030515     c                   movel(p)  §3Atb2        TBLkey
411900030515x   2c                   else
412000030515     c                   movel(p)  §3Atb1        TBLkey
412100030515e   2c                   endif
412200030515     c     K03TAB        chain     TABEL
412300030515if  2c                   if        %found(TABEL00F)
412400030515     c                   movel     TBLuni        dsTB
412500030515e   2c                   endif
412600030515e   1c                   endif
412700030515      *
412800030515      * Tab. "5E" - Tipi Servizio
412900030515     c                   clear                   ds5E
413000090514if  1c                   if        DB0tsp <> *blanks
413100030515     c                   movel     '5E'          TBLcod
413200030515     c                   movel(p)  DB0tsp        TBLkey
413300030515     c     K03TAB        chain     TABEL
413400030515if  2c                   if        %found(TABEL00F)
413500030515     c                   movel     TBLuni        ds5E
413600030515e   2c                   endif
413700030515e   1c                   endif
413800030515      *
413900030911      * Tab. "EI" - Esenzioni I.V.A.
414000030515     c                   clear                   dsEI
414100030911      * Tab. "1A" - Tipi incasso contrassegno
414200030911     c                   clear                   ds1A
414300030911      * Tab. "1P" - Consegne particolari
414400030911     c                   clear                   ds1P
414500030515      *
414600030515     c                   ENDSR
414700030911      *
414800030911      *---------------------------------------------------------------*
414900050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
415000030911      *---------------------------------------------------------------*
415100030911     c     Tab_EI        BEGSR
415200030911      *
415300030911     c                   clear                   dsEI
415400030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
415500030911     c                   movel     'EI'          TBLcod
415600030911     c                   movel(p)  DB0cei        TBLkey
415700030911     c     K03TAB        chain     TABEL
415800030911if  2c                   if        %found(TABEL00F)
415900030911     c                   movel     TBLuni        dsEI
416000030911e   2c                   endif
416100030911e   1c                   endif
416200030911     c                   eval      DB0dce =  §EIdes
416300030911      *
416400030911     c                   ENDSR
416500030515      *
416600030515      *---------------------------------------------------------------*
416700050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
416800030515      *---------------------------------------------------------------*
416900030515     c     Tab_1A        BEGSR
417000030515      *
417100030515     c                   clear                   ds1A
417200030515if  1c                   if        DB0cas <> *zeros
417300030515     c                   movel     '1A'          TBLcod
417400030515     c                   movel(p)  DB0tic        TBLkey
417500030515     c     K03TAB        chain     TABEL
417600030515if  2c                   if        %found(TABEL00F)
417700030515     c                   movel     TBLuni        ds1A
417800030515e   2c                   endif
417900030515e   1c                   endif
418000030515      *
418100030515      * Imposto i dati nella DS
418200030515     c                   eval      DB0dti = §1Ades
418300030515     c                   eval      DB0ucm = §1Aucm
418400030515     c                   eval      DB0ste = §1Aste
418500030515      *
418600030515     c                   ENDSR
418700030515      *
418800030515      *---------------------------------------------------------------*
418900050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
419000030515      *---------------------------------------------------------------*
419100030515     c     Tab_1P        BEGSR
419200030515      *
419300030515     c                   clear                   ds1P
419400030515     c                   movel     '1P'          TBLcod
419500130924     c***?già impostato:?movel(p)  DB0tc1/2      TBLkey
419600030515     c     K03TAB        chain     TABEL
419700030515if  1c                   if        %found(TABEL00F)
419800030515     c                   movel     TBLuni        ds1P
419900030515e   1c                   endif
420000030515      *
420100030515     c                   ENDSR
420200030515      *
420300030515      *---------------------------------------------------------------*
420400050217      *?Calcolo importi LDV                                          ?*
420500030515      *---------------------------------------------------------------*
420600061031     c     sr_DS_IMPO    BEGSR
420700030515      *
420800030515      * Vedo se esiste tassazione
420900030515      *  solo se tipo bolla contabilizzabile
421000061030if  1c                   IF        §TBfcb =  '1'
421100101213      *
421200061030     c                   eval      Wtrc   =  '1'
421300040607     c     KLDVtrc       chain     FIAR6000
421400101213      *
421500030515if  2c                   if        %found(FIAR601L)
421600030515     c                   movel     AR6div        DB0div
421700030515     c                   exsr      CtrDIV
421800030515      *
421900030515      * NON c'è 2ª bolla
422000101213if  3c                   if        DB0tb2 = *blanks
422100030515      *  Estremi fattura
422200030515     c                   z-add     AR6nft        DB0nft
422300030515     c                   z-add     AR6dft        DB0dft
422400130205     c                   z-add     AR6fiv        wB0fiv
422500101213      *  Se è assegnato => stampo tutta la tassaz.
422600101213if  4c                   if        %subst(DB0tb1:1:1) = 'A'
422700061030     c                   eval      Wtrc   =  '1'
422800030515     c                   exsr      RepTASSAZ
422900030515e   4c                   endif
423000030515e   3c                   endif
423100030515e   2c                   endif
423200030515      *
423300061030      * C'è 2ª bolla
423400101213if  2c                   IF        DB0tb2 <> *blanks
423500030515      *
423600061030     c                   eval      Wtrc   =  '2'
423700040607     c     KLDVtrc       chain     FIAR6000
423800030515      *
423900061030if  3c                   If        %found(FIAR601L)
424000030515     c                   movel     AR6div        DB0div
424100030515     c                   exsr      CtrDIV
424200030515     c                   movel     AR6cei        DB0cei
424300030515      *   Estremi fattura
424400030515     c                   z-add     AR6nft        DB0nft
424500030515     c                   z-add     AR6dft        DB0dft
424600130205     c                   z-add     AR6fiv        wB0fiv
424700030515if  4c                   if        AR6ksc <> *zeros
424800030515     c                   eval      DB0ccd =  AR6ksc
424900030515e   4c                   endif
425000061030     c                   eval      Wtrc   =  '2'
425100030515     c                   exsr      RepTASSAZ
425200061030e   3c                   EndIf
425300030515      *
425400030515e   2c                   ENDIF
425500030515      *
425600030515e   1c                   ENDIF
425700030515      *
425800030515      * Vedo se ha contrassegno
425900060505     c     KLDV          chain     FIAR9000
426000060505if  1c                   IF        %found(FIAR901L)
426100061030     c                   eval      DB0cas =  AR9cas
426200061030     c                   eval      DB0vca =  AR9vca
426300061030     c                   eval      DB0gca =  AR9gca
426400061030     c                   eval      DB0tic =  AR9tic
426500030515x   1c                   ELSE
426600030515     c                   clear                   DB0cas
426700030515     c                   clear                   DB0vca
426800030515     c                   clear                   DB0gca
426900030515     c                   clear                   DB0tic
427000030515     c                   clear                   DB0dti
427100030515     c                   clear                   DB0ucm
427200030515     c                   clear                   DB0ste
427300030515e   1c                   ENDIF
427400121018      *
427500131015      * Se P.A. con fattura immediata riporterò il Codice Fiscale
427600121018      *   (NON se destinatario estero)
427700121026if  1c                   if           ( %subst(DB0tb1:1:1) = 'A'
427800121018     c                             and (DB0nft  <> *zeros
427900121018     c                              or  DB0ift  <> *zeros)
428000121018     c                             and  DB0nzd  =  *blanks )
428100121018     c                   exsr      RepAR4_Q
428200121018     c                   movel     §TA4Qcfd      DB0cpi
428300121018e   1c                   endif
428400121018     c                   eval      wB0cfi = DB0cpi
428500061030      *
428600030515      * Vedo se si tratta di bolla figlia:
428700040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
428800070515if  1c                   if        $Mamma =  *on
428900030911     c                   exsr      BollaFiglia
429000030515e   1c                   endif
429100030515      *
429200030515     c                   ENDSR
429300030515      *
429400030515      *---------------------------------------------------------------*
429500050217      *?Decodifica LNA                                               ?*
429600030515      *---------------------------------------------------------------*
429700131203     c     sr_DecLNA     BEGSR
429800030515      *
429900030515if  1c                   if            DB0dla =  *blanks
430000030515     c                             and DB0ris <> 'M'
430100030515      *
430200040311     c                   eval      SAVlna = DB0lna
430300070312     c                   clear                   OG146arr
430400061016     c                   clear                   OG148arr
430500070312     c                   clear                   OG150arr
430600030515     c     DB0lna        chain     AZORG
430700030515      *
430800030515if  2c                   if        %found(AZORG01L)
430900030515     c                   eval      DB0fag = ORGfag
431000030515     c                   eval      DB0dla = ORGdes
431100030515     c                   eval      DB0inl = ORGind
431200030515     c                   eval      DB0cal = ORGcpf
431300030515     c                   eval      DB0lol = ORGloc
431400030515     c                   eval      DB0prl = ORGpro
431500030515     c                   eval      DB0tel = ORGtel
431600030515     c                   eval      DB0fax = ORGfax
431700070312     c                   movel     ORGde6        OG146arr
431800061016     c                   movel     ORGde8        OG148arr
431900070312     c                   movel     ORGdf0        OG150arr
432000030515e   2c                   endif
432100030515      *
432200030515e   1c                   endif
432300030515      *
432400030515     c                   ENDSR
432500030515      *
432600030515      *---------------------------------------------------------------*
432700050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
432800030515      *---------------------------------------------------------------*
432900030515     c     RepTASSAZ     BEGSR
433000030515      *
433100030515     c                   eval      DB0imv = AR6imv
433200030515     c                   eval      DB0cei = AR6cei
433300030515     c                   eval      DB0ift = AR6ift
433400101213      *
433500030911      * Esenzione IVA
433600030911if  1c                   if            DB0cei <> *blanks
433700030911     c                             and DB0dce =  *blanks
433800030911     c                   exsr      Tab_EI
433900030911e   1c                   endif
434000101213      *
434100101213      * Carico tabella VARIABILI ARRIVI
434200101213     c                   clear                   ds$2
434300101213     c                   eval      TBLcod = '$2'
434400101213     c                   eval      TBLkey = '1'
434500101213     c     K03TAB        chain     TABEL
434600101213if  1c                   if        %found(TABEL00F)
434700101213     c                   movel     TBLuni        ds$2
434800101213e   1c                   endif
434900030515      *
435000030515      * Controllo se ci sono varie esenti
435100030515      *
435200030515      * - varia 1
435300030515     c                   eval      Wsvx = AR6sv1
435400030515     c                   eval      Wvax = AR6va1
435500030515     c                   exsr      RepTASSAZ_b
435600030515      * - varia 2
435700030515     c                   eval      Wsvx = AR6sv2
435800030515     c                   eval      Wvax = AR6va2
435900030515     c                   exsr      RepTASSAZ_b
436000030515      * - varia 3
436100030515     c                   eval      Wsvx = AR6sv3
436200030515     c                   eval      Wvax = AR6va3
436300030515     c                   exsr      RepTASSAZ_b
436400030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
436500040607     c     KLDVtrc       setll     FIAR7000
436600040607     c     KLDVtrc       reade     FIAR7000
436700030515      *
436800030515do  2c                   Dow       Not %eof(FIAR701L)
436900030515      *
437000030515     c                   eval      Wsvx = AR7svn
437100030515     c                   eval      Wvax = AR7van
437200030515     c                   exsr      RepTASSAZ_b
437300030515      *
437400040607     c     KLDVtrc       reade     FIAR7000
437500030515e   2c                   Enddo
437600030515      *
437700030515     c                   ENDSR
437800030515      *
437900030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
438000050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
438100050217      *?- 2ª parte -                                                 ?*
438200030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
438300030515     c     RepTASSAZ_b   BEGSR
438400030515      *
438500030515sel 1c                   Select
438600030515      *
438700030515w   1c                   When      Wsvx       = *blanks
438800030515      *
438900030515w   1c                   When      Wsvx       = §$2iva
439000030515     c                   eval      DB0iva     = Wvax
439100030515      *
439200030515w   1c                   When      Wsvx       = §$2bol
439300030515     c                   add       Wvax          DB0ese
439400110117     c*//                z-add     Wvax          wBolloVirt
439500110117     c*//                eval      $BolloVirt = *on
439600030515      *
439700030515x   1c                   Other
439800101213      * Reperimento varie esenti IVA
439900101213     c                   movel     'CC'          TBLcod
440000101213     c                   movel(p)  Wsvx          TBLkey
440100101213     c     K03TAB        chain     TABEL
440200101213if  1c                   IF             %found(TABEL00F)
440300101213     c                             and  TBLflg = *blanks
440400101213     c                   movel     TBLuni        dsCC
440500101213if  2c                   if        §CCcei <> *blanks
440600101213     c                   add       Wvax          DB0ese
440700101213     c                   sub       Wvax          DB0imv
440800101213e   2c                   endif
440900101213e   1c                   ENDIF
441000030515      *
441100101213e   1c                   Endsl
441200030515      *
441300030515     c                   ENDSR
441400030515      *
441500030515      *---------------------------------------------------------------*
441600050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
441700030515      *---------------------------------------------------------------*
441800030515     c     TABxPAR       BEGSR
441900030515      *
442000100517     c                   movel(p)  WCdPar        w053a
442100030515     c                   movel     '7V'          TBLcod
442200030515     c                   movel(p)  KeyPAR        TBLkey
442300030515     c     K03TAB        chain     TABEL
442400030515      *
442500030515if  1c                   if        %found(TABEL00F)
442600030515     c                             and TBLflg = *blanks
442700100517     c                   eval      w053a = WCdPar + '-'
442800121108     c                                   + %trim(%subst(TBLuni:1:49))
442900030515x   1c                   else
443000030515     c                   clear                   TBLuni
443100030515e   1c                   endif
443200030515      *
443300030515     c                   ENDSR
443400030515      *
443500030515      *---------------------------------------------------------------*
443600050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
443700030515      *---------------------------------------------------------------*
443800030515     c     CtrCAs        BEGSR
443900030515      *
444000030515      * E' da convertire
444100030515if  1c                   if        DB0ucm = 'S'
444200030515     c                   eval      $CAeur = *on
444300030515e   1c                   endif
444400030515      *
444500030515      * Se in EURO è da convertire e da stampare in EURO
444600030515if  1c                   if        DB0vca = §GEDcn
444700030515     c                   eval      $CAeur = *on
444800030515     c                   endif
444900030515      *
445000030515      * Devo convertire il c/a da valuta a moneta di conto
445100030624if  1c                   if        DB0vca <> §GEDcn
445200030515     c                   clear                   YEURCODS
445300030515     c                   movel     DB0vca        YECdvi
445400030515     c                   z-add     DB0cas        YECimi
445500030515     c                   movel     §GEDcn        YECdvo
445600030515     c                   call      'YEURCO'
445700030515     c                   parm                    YEURCODS
445800030515if  2c                   if        YECesi = *blanks
445900030515     c                   z-add     YECimo        Wcas
446000030515e   2c                   endif
446100030515x   1c                   else
446200030515     c                   z-add     DB0cas        Wcas
446300030515e   1c                   endif
446400030515      *
446500030515     c                   ENDSR
446600030515      *
446700030515      *---------------------------------------------------------------*
446800050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
446900030515      *---------------------------------------------------------------*
447000030515     c     CtrDIV        BEGSR
447100030515      *
447200030515     c                   clear                   dsCV
447300030515      *
447400030515     c                   movel     'CV'          TBLcod
447500030515     c                   movel(p)  DB0div        TBLkey
447600030515     c     K03TAB        chain     TABEL
447700030515     c                   if        %found(TABEL00F)
447800030515     c                   movel     TBLuni        dsCV
447900030515     c                   movel     §CVfdc        DB0fdc
448000030515     c                   endif
448100030515      *
448200030515     c                   ENDSR
448300030515      *
448400030515      *---------------------------------------------------------------*
448500050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
448600030515      *---------------------------------------------------------------*
448700030911     c     BollaFiglia   BEGSR
448800030515      *
448900090619     c                   if        not %open(FNLBL01L)
449000101213     c                   open(e)   FNLBL01L
449100101213      /free
449200101213           if  %error;
449300101213             exsr  sr_ErrOpenFile;
449400101213           endif;
449500101213      /end-free
449600090619     c                   endif
449700090619      *
449800040607     c     KLDV          chain     FNLBL000
449900030515if  1c                   if        %found(FNLBL01L)
450000030515      *
450100030515     c     KLBL          chain     FNARB000
450200030515if  2c                   if        %found(FNARB01L)
450300030515      *
450400031015     c                   eval      wB0nrs = ARBnrs
450500030515     c                   eval      DB0fns = ARBfns
450600030515     c                   eval      DB0fls = ARBfls
450700030515     c                   eval      DB0ncd = ARBncd
450800030515     c                   eval      DB0nca = ARBnca
450900050114     c**!!!              eval      DB0xco = ARBxco
451000030515     c                   eval      DB0znc = ARBznc
451100030515      * Aggancio AR9
451200060505     c     KLBL          chain     FIAR9000
451300060505if  3c                   IF        %found(FIAR901L)
451400030515     c                   eval      DB0cas = AR9cas
451500030515     c                   eval      DB0vca = AR9vca
451600030515     c                   eval      DB0gca = AR9gca
451700030515     c                   eval      DB0tic = AR9tic
451800030515x   3c                   ELSE
451900030515     c                   clear                   DB0cas
452000030515     c                   clear                   DB0vca
452100030515     c                   clear                   DB0gca
452200030515     c                   clear                   DB0tic
452300030515     c                   clear                   DB0dti
452400030515     c                   clear                   DB0ucm
452500030515     c                   clear                   DB0ste
452600030515e   3c                   ENDIF
452700030515      *
452800030515e   2c                   endif
452900030515      *
453000030515e   1c                   endif
453100030515      *
453200030515     c                   ENDSR
453300030903      *
453400030515      *---------------------------------------------------------------*
453500050217      *?DEFINIZIONE KEY-LIST                                         ?*
453600030515      *---------------------------------------------------------------*
453700030515     c     DefKLIST      BEGSR
453800030515      *
453900090619      * - FNARB01L / FNART01L /
454000030515      *   FNLBL01L /
454100060505      *   FIAR501L/31C / FIAR901L
454200040607     c     KLDV          klist
454300030515     c                   kfld                    DB0aas
454400030515     c                   kfld                    DB0lnp
454500030515     c                   kfld                    DB0nrs
454600030515     c                   kfld                    DB0nsp
454700060505      * - FIAR401L / FIAR601L / FIAR701L
454800060505     c     KLDVtrc       klist
454900060505     c                   kfld                    DB0aas
455000060505     c                   kfld                    DB0lnp
455100060505     c                   kfld                    DB0nrs
455200060505     c                   kfld                    DB0nsp
455300060505     c                   kfld                    Wtrc
455400040607      * - FIAR501L/31C
455500030717     c     K05AR501      klist
455600030717     c                   kfld                    DB0aas
455700030717     c                   kfld                    DB0lnp
455800030717     c                   kfld                    DB0nrs
455900030717     c                   kfld                    DB0nsp
456000030717     c                   kfld                    AR5trd
456100090619      * - FNARB01L / FIAR901L di bolla MAMMA
456200070514     c     KLBL          klist
456300070514     c                   kfld                    LBLaap
456400070514     c                   kfld                    LBLlpp
456500070514     c                   kfld                    LBLnrp
456600070514     c                   kfld                    LBLnsp
456700081030      * - FNART01L di bolla MAMMA
456800081030     c     KLBLnsc       Klist
456900081030     c                   kfld                    LBLaap
457000081030     c                   kfld                    LBLlpp
457100081030     c                   kfld                    LBLnrp
457200081030     c                   kfld                    LBLnsp
457300081030     c                   Kfld                    Wnsc
457400081030     c     K05ARS01A     klist
457500081030     c                   kfld                    ARTfls
457600081030     c                   kfld                    ARTlna
457700081030     c                   kfld                    ARTnrs
457800081030     c                   kfld                    ARTnsc
457900081030     c                   kfld                    ARStrc
458000081030      * - FNART01L
458100081030     c     KLDVnsc       Klist
458200081030     c                   Kfld                    DB0aas
458300081030     c                   Kfld                    DB0lnp
458400081030     c                   Kfld                    DB0nrs
458500081030     c                   Kfld                    DB0nsp
458600081030     c                   Kfld                    Wnsc
458700100517      * - FNORM01L
458800100517     c     K04ORM01      klist
458900100517     c                   kfld                    ORMpoe
459000100517     c                   kfld                    ORMnsr
459100100517     c                   kfld                    ORMnor
459200100517     c                   kfld                    ORMnrv
459300030515      * - TABEL00F
459400030515     c     K03TAB        klist
459500040223     c                   kfld                    TBLkut
459600030515     c                   kfld                    TBLcod
459700030515     c                   kfld                    TBLkey
459800030515      *
459900030515     c                   ENDSR
460000080430
460100080430       //--------------------------------------------------------------
460200140303       //?Operazioni finali                                            ?
460300080430       //--------------------------------------------------------------
460400080430     c     sr_End        BEGSR
460500080430      *
460600080430     c                   clear                   TIBS02ds
460700080430     c                   eval      T02tla = 'C'
460800080430     c                   call      'TIBS02R'
460900080430     c                   parm                    KPJBA
461000080430     c                   parm                    TIBS02ds
461100080430     c                   reset                   FNLV80ds
461200080430     c                   call      'FNLV80R'
461300080430     c                   parm                    KPJBA
461400080430     c                   parm                    FNLV80ds
461500080430      /free
461600101222         if  §VPOrcm <= wDate and
461700101222             DB0tsp   = 'E'   and
461800101222            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
461900101222                                   DB0lnp <> 006   and
462000101222                                   DB0lnp <> 063));
462100101222           reset  TNSD99ds;
462200101222           tnsd99r ( TNSD99ds );
462300110117         endif;
462400080724
462500101222         // -?Chiusura del work-file?- - - - - - - - - - - - - - - - -*
462600090210
462700110202         if  %open(FIILV01L);
462800110202           close  FIILV01L;
462900090210         endif;
463000080430
463100101222         // -?Restituzione parametri?- - - - - - - - - - - - - - - - -*
463200080508
463300110221         if wCod_Log  <> '01';
463400130206           RootFolder = §ILVimgSca;
463500110221         endif;
463600110221
463700080520         if Risultato <> 'OK';
463800080718
463900080731           clear FileName;
464000080731           clear TemplateName;
464100080508
464200130213           for wx = 1 to c_Row;
464300130206             %occur(LdV) = wx;
464400130206             LdvPrg      = wx;
464500130206           endfor;
464600080508
464700080508         endif;
464800080430
464900131203
465000080430           exec sql   set result sets
465100080430                      for return to client
465200081103                      array :LdV   for :w_row rows;
465300080430
465400080430           select;
465500080430             when sqlCode = 100;
465600080430               RtnStatus  = sqlCode;
465700130924               //RtnOpCode  = 'SQLCODE   ';
465800130924               //RtnEsito   = -1;
465900080430             when sqlCode < *zero;
466000080430               RtnStatus  = sqlCode;
466100130924               //RtnOpCode  = 'SQLCODE   ';
466200130924               //RtnEsito   = -1;
466300080430           endsl;
466400080430
466500080430
466600080430         return;
466700080430
466800080430      /end-free
466900080430     c                   ENDSR
467000080430
467100080430      *---------------------------------------------------------------*
467200130227
467300140303       //--------------------------------------------------------------
467400140303       //?Spool di stampa (per e-mail).                                ?
467500140303       //--------------------------------------------------------------
467600130227
467700130227     oPrtEMAIL  e            EmlTxt            2
467800130227     o                       SDSpgm
467900130227     o                                        +   1 '-'
468000130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
468100130227     o                                              ABORATA 100 VOLTE'
468200130227     o          e            EmlTxt      0
468300130227     o                       SDSpgm
468400130227     o                                        +   1 '-'
468500130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
468600130227     o                                              ABORATA 100 VOLTE'
468700130227     o          e            EmlTxt      2
468800130227     o                                              'Spedizione     '
468900130227     o                                        +   3 'Distinta  '
469000130227     o          e            EmlTxt      1  1
469100130227     o                                              '------------------
469200130227     o                                              --'
469300130227     o                                        +   3 '----------'
469400130227     o          e            EmlDet      1
469500130227     o                       DB0aas        3
469600130227     o                       DB0lnp        3  +   1
469700130227     o                       DB0nrs        3  +   1
469800130227     o                       DB0nsp        3  +   1
469900130227     o                       P_Fgs2        3  +   3
470000130227     o                       P_Ndc2        3  +   1
470100130227     o          e            EmlEnd      2
470200130227     o                                              '***  Fine Lista  ***'
470300130227     o          e            EmlEnd      0
470400130227     o                                              '***  Fine Lista  ***'
470500080724
470600140303       //--------------------------------------------------------------
470700140303       //?Spool per immagine della LdV.                                ?
470800140303       //--------------------------------------------------------------
470900080430     oQSYSPRT   e            PRTtxt            3
471000080430     o                                              '   '
471100080430     o                       c_Char           +   0
471200080430     o          e            PRTrow      1
471300080430     o                       LDV              +   0
471400110201     o          e            PRTend      3
471500110201     o                       LastRow          +   0
471600080430
471700080718       //--------------------------------------------------------------
471800140303       //?Procedure name: getCurrentSystemName.                        ?
471900140303       //?Purpose:        Reperisce l'attributo di rete current System Name?
472000140303       //?Returns:        Current System Name                          ?
472100080718       //--------------------------------------------------------------
472200080718     p getCurrentSystemName...
472300080718     p                 b
472400080718
472500080718     d getCurrentSystemName...
472600080718     d                 pi             8a
472700080718
472800080718      /free
472900080718
473000080718       clear qUSEC;
473100080718       qUSBPRV = %size(qUSEC);
473200080718
473300080718       RetrieveNetworkAttributes( QWCRNETA_dataReturned
473400080718                                : %size(qWCRNETA_dataReturned)
473500080718                                : 1
473600080718                                : 'SYSNAME   '
473700080718                                : qUSEC
473800080718                                );
473900080718
474000080718       if  qUSEI <> *blank   or
474100080718           qWCRNETA_dataReturned.qWCRDR.qWCNBRAR = *zero;
474200080718         dump(A);
474300080718         clear qWCRNETA_networkAttributeInformationTable;
474400080718       else;
474500080718         qWCRNETA_networkAttributeInformationTable =
474600080718                                       qWCRNETA_dataReturned.qWCARRAY;
474700080718       endif;
474800080718
474900080718       return  qWCRNETA_networkAttributeInformationTable.qWCDATA;
475000080718
475100080718      /end-free
475200080718
475300080718     p getCurrentSystemName...
475400080718     p                 e
475500080718
475600110202       //--------------------------------------------------------------
475700110202       //?Schiere a tempo di compilazione.                             ?
475800110202       //--------------------------------------------------------------
475900030515
476000030515** > $GgS/$Gio * --------------------------------------------------------------*
4761000306241LUNEDI
4762000306242MARTEDI
4763000410263MERCOLEDI
4764000306244GIOVEDI
4765000306245VENERDI
4766000305156SABATO
4767000305157DOMENICA
476800030515M          MATT.
476900030515P          POM.
477000110202** -?$CoL?(Codice LOG)?/?$DeL?(Decodifica codice LOG)?
47710011020200Spedizione in distinta chiusa - da elaborare
47720011020201Spedizione elaborata
47730011032808Sped. non consegnata in dist. - immagine a SCARTO
47740011032509LdV da scannerizzare - immagine a SCARTO
47750011020321Parametri di input errati - immagine NON creata
47760011020822Distinta non trovata/annullata - immagine a SCARTO
47770011020823Sped./Dist. non trovata in FIILV - immag. a SCARTO
47780011020325Sped. non trovata in FNARB - immagine NON creata
47790013031351BOLLA RECUPERO - Immagine già generata
47800013031352BOLLA RECUPERO - Spedizione non trovata in FNARB
