000100101220       //==============================================================
000200101220       //?  STORE PROCEDURE - DATI PER IMMAGINI LDV (TERMINALINI AUT)  ?
000300130411       //?                    (ANCHE RIGENERAZIONE)                    ?
000400101220       //==============================================================
000500101220       //
000600101220       //? QUESTO PROGRAMMA È "LEGATO"? A "FNLSB6R" (STAMPA LDV A4).  ?
000700130214       //? VERIFICARE SE LA MODIFICA E' DA APPORTARE ANCHE AD ESSO !!  ?
000800101220       //
000900130924       //ATTENZIONE:?I CAMPI NEL RECORD LSB6LDV RISULTANO IMPOSTATI
001000101220       //             NELLE VARIE STRUTTURE DATI FISP30nnDS - TUTTI
001100101220       //             ALFANUMERICI !
001200130924       //
001300130924       //NOTA BENE (a):?Valori previsti nel parm di output RISULTATO:?
001400130924       //               ?· 1) "OK" => Generare l'immagine             ?
001500130924       //               ?· 2) "NO" => Ripetere la richiesta           ?
001600130924       //               ?· 3) "XX" => NON ripetere la richiesta       ?
001700101220       //
001800130924       //NOTA BENE (b):?> Cod. cliente 0501331 => FedEx
001900150430       //                  cod. fissato nella costante "c_CliFedEx"
002000150430       //                > Codice cliente 0506514 => FedEx (2)
002100150430       //                  codice fissato nella costante "c_CliFedEx2"
002200130924       //                > Cod. cliente 0201515 => PraOil oleodotti
002300150430       //                  cod. fissato nella costante "c_CliPraOil"
002400130924       //                > Cod. cliente 0504450 => TTL
002500130924       //                  cod. fissato nella costante "c_CliTTL"
002600130924       //                > Cod. cliente 0632910 => Consorzio Lotterie Naz
002700130924       //                  cod. fissato nella costante "c_CliCLN"
002800140513       //                > Cod. cliente 0017093 => Lotterie Nazionali
002900140513       //                       (gemello di backup su Bologna)
003000140513       //                  cod. fissato nella costante "c_CliLN_2"
003100130924       //                > Cod. cliente 0634923 => S G Soluzioni Logist.
003200130924       //                  cod. fissato nella costante "c_CliSG "
003300130924       //                > Cod. cliente 0635042 => S G Soluzioni Logist.
003400130924       //                  cod. fissato nella costante "c_CliSG_2"
003500101220       //
003600101220       //==============================================================
003700101213
003800101213       //--------------------------------------------------------------
003900101213       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
004000101213       //--------------------------------------------------------------
004100101213
004200130411     /*PRM  dbgview(*source)
004300130411     /*PRM  commit(*NONE)
004400130411     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
004500130411     /*CMD         ovrscope(*calllvl)
004600130411     /*END  dltovr file(TITAS30C) lvl(*)
004700101213     /*END
004800101213
004900101213       //--------------------------------------------------------------
005000101213       //?Specifiche di controllo.                                     ?
005100101213       //--------------------------------------------------------------
005200080430
005300080430     h decedit('0,') datedit(*dmy/) option(*nodebugio)
005400080718     h dftactgrp(*no) actgrp(*caller)
005500080430
005600101213       //--------------------------------------------------------------
005700101213       //?Dichiarazione file.                                          ?
005800101213       //--------------------------------------------------------------
005900080430
006000101213       // -?Tabelle?
006100101213     fTABEL00F  if   e           k disk    usropn
006200101213
006300101213       // -?Organigramma di filiale?
006400101213     fAZORG01L  if   e           k disk    usropn
006500130411
006600130411       // -?Distinte delle cui LdV ri-generare le immagini?
006700130422     fFIIRD11L  if   e           k disk
006800130411
006900130411       // -?Dati per LdV (sede)?
007000130411     fTITAS30C  if   e           k disk    usropn
007100130411     f                                     extfile(wLibFileTITAS)
007200101213
007300101213       // -?Dati per LdV arrivo?
007400070305     fFNARB01L  if   e           k disk    usropn
007500070306     fFNART01L  if   e           k disk    usropn
007600131206     fFNEVB04L  if   e           k disk    usropn
007700140303     fFNEVB22L  if   e           k disk    usropn
007800080710     fFIDST01L  if   e           k disk    usropn
007900101213
008000101213       // -?Altri dati comuni?
008100090619     fFNLBL01L  if   e           k disk    usropn
008200081030     fFIARS01L  if   e           k disk    usropn
008300070305     fFIAR401L  if   e           k disk    usropn
008400030515     fFIAR501L  if   e           k disk    usropn
008500070305     fFIAR601L  if   e           k disk    usropn
008600070305     fFIAR701L  if   e           k disk    usropn
008700070305     fFIAR901L  if   e           k disk    usropn
008800170517     fTFNTC01L  if   e           k disk    usropn
008900171117
009000171117       // -?Bolle: estensione DPD?
009100171117     fFIPND01L  if   e           k disk    usropn
009200110117
009300131017       // -?Dati per LdV partenza (solo per bolla mamma - cliente CLN)?
009400110117     fFNBLP01L  if   e           k disk    usropn
009500101213
009600101207       // -?O.R.M.?
009700100517     fFNORM01L  if   e           k disk    usropn
009800101213
009900110309       // -?File LOG spedizioni elaborate per immagine LdV?
010000110202     fFIILV01L  Uf A e           k disk    usropn
010100130227
010200130227       // -?Spool da inviare via e-mail:?
010300130227       //   ·?Corpo del messaggio di posta elettronica?
010400130227     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOE)
010500101213
010600101207       // -?Printer File  (se richiesta bozza in stampa)?
010700090210     fQSYSPRT   o    f  132        printer usropn  oflind(*inOF)
010800030515
010900101213       //--------------------------------------------------------------
011000101213       //?Definizione costanti.                                        ?
011100101213       //--------------------------------------------------------------
011200030515
011300110117     d c_DateMax_A     c                   const('20391231')
011400170517       // -?Capoconto clienti?
011500170517     d c_Kci           c                   const(0151)
011600131008       // -?Costanti in output?
011700110117     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
011800030515     d                                     NON SEQUENZIALI')
011900110117     d c_CoOr          c                   const('COLLI ORIGINALI     -
012000030717     d                                     N. ')
012100110117     d c_PMxA          c                   const('PROMEMORIA PER L''AU-
012200070315     d                                     TOTRASPORTATORE')
012300101213       // -?Codice cliente "FedEx"?
012400150430     d c_CliFedEx      c                   const(0501331)
012500150430     d c_CliFedEx2     c                   const(0506514)
012600101213       // -?Codice cliente "PraOil oleodotti italiani"?
012700150430     d c_CliPraOil     c                   const(0201515)
012800101213       // -?Codice cliente "TTL"?
012900091116     d c_CliTTL        c                   const(0504450)
013000101213       // -?Codice cliente "Lotterie Nazionali - Gratta & Vinci"?
013100090619     d c_CliCLN        c                   const(0632910)
013200140513     d c_CliLN_2       c                   const(0017093)
013300101213       // -?Codice cliente "S G Soluzioni Logistiche"?
013400090619     d c_CliSG         c                   const(0634923)
013500091117     d c_CliSG_2       c                   const(0635042)
013600110923
013700110923       // -?Consegna CON Riserva specifica?
013800110923     d c_RiservaSpecif...
013900110923     d                 c                   const('RS')
014000091117
014100131008       // -?Costante per evitare l'indicazione del contratto x "S.G."?
014200091117     d c_NoContr       c                   const('NO CONTRATTO')
014300101213
014400101213       // -?Costante per controllo "caratteri solo numerici"?
014500110126     d c_Digits        c                   const('0123456789')
014600101207       // -?Numero di righe da gestire per la LdV?
014700130213     d c_Row           c                   const(36)
014800081027     d c_Row_B         c                   const(49)
014900130213     d c_Row_B2        c                   const(51)                            c_Row_B + 2
015000081030     d c_Row_Fed       c                   const(10)
015100101207       // -?Sequenza numerica?
015200080430     d c_Char          c                   const('*...+....1....+....2+
015300080430     d                                            ....+....3....+....4+
015400080430     d                                            ....+....5....+....6+
015500080430     d                                            ....+....7....+....8+
015600080430     d                                            ....+....9....+...10+
015700080430     d                                            ....+...11....+...12')
015800130227
015900130227       // -?Dati di default?
016000130227     d c_Mittente      c                   const('ced')
016100130227     d c_at            c                   const('@brt.it')
016200130227     d c_Sede          c                   const('046')
016300130227
016400130227       // -?Comandi di override al PrtF "corpo e-mail"?
016500130227     d c_CmdOvrPrtF    c                   const('OVRPRTF +
016600130227     d                                           file(PRTEMAIL) +
016700130227     d                                           pagesize(66 132) +
016800130227     d                                           lpi(6) cpi(10) +
016900130227     d                                           ovrscope(*actgrpdfn) +
017000130227     d                                           ')
017100130227     d c_CmdDltOvr     c                   const('DLTOVR +
017200130227     d                                            file(PRTEMAIL) +
017300130227     d                                            lvl(*actgrpdfn)')
017400101213
017500101213       //--------------------------------------------------------------
017600101213       //?Definizione schiere.                                         ?
017700101213       //--------------------------------------------------------------
017800101213
017900101213       // -?Giorni della settimana e relativa decodifica?
018000070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
018100070306     d $Gio            s             15    dim(9)   alt($GgS)
018200110202
018300110202       // -?Codice / Decodifica LOG x FIILV00F?
018400110202     d $CoL            s                   like(ILVcol)
018500130313     d                                     dim(10)  ctdata  perrcd(1)
018600110202     d $DeL            s                   like(ILVnot)
018700130313     d                                     dim(10)  alt($CoL)
018800101213
018900101207       // -?Dati da restituire?
019000130214     d LdV             ds           123    occurs(c_Row_B2) inz
019100080508     d   LdvPrg                       3s 0 inz
019200080430     d   LdvTxt                     120    inz
019300101213
019400101213       // -?Note e Particolarità?
019500070306     d $NP             s                   dim(6)   inz  like(Pnp1)
019600101213       // -?Consegne Precedenti?
019700070319     d $CP             s                   dim(20)  inz(*all'9')
019800070319     d                                                   like(Elem_CP)
019900070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
020000101213
020100101213       //--------------------------------------------------------------
020200101213       //?Definizione strutture dati.                                  ?
020300101213       //--------------------------------------------------------------
020400101213
020500080430     d KPJBA         e ds                  inz
020600101213
020700131008       // -?Dati LDV da restituire per l'immagine?
020800080506     d FNLSB5ds      e ds                  inz
020900080430
021000110202       // -?Records in output?
021100110202       // ·? 1ª riga?
021200081021     d ds_01         e ds                  inz  extname(FISP3001ds)
021300081021     d                                          prefix(P : 3)
021400110202       // ·? 2ª riga (vuota)?
021500110202       // ·? 3ª riga?
021600080430     d ds_03         e ds                  inz  extname(FISP3003ds)
021700080430     d                                          prefix(P : 3)
021800110202       // ·? 4ª riga?
021900080430     d ds_04         e ds                  inz  extname(FISP3004ds)
022000080430     d                                          prefix(P : 3)
022100110202       // ·? 5ª riga?
022200080430     d ds_05         e ds                  inz  extname(FISP3005ds)
022300080430     d                                          prefix(P : 3)
022400110202       // ·? 6ª riga (vuota)?
022500110202       // ·? 7ª riga?
022600080430     d ds_07         e ds                  inz  extname(FISP3007ds)
022700080430     d                                          prefix(P : 3)
022800110202       // ·? 8ª riga (vuota)?
022900110202       // ·? 9ª riga?
023000080430     d ds_09         e ds                  inz  extname(FISP3009ds)
023100080430     d                                          prefix(P : 3)
023200110202       // ·?10ª riga?
023300080430     d ds_10         e ds                  inz  extname(FISP3010ds)
023400080430     d                                          prefix(P : 3)
023500110202       // ·?11ª riga?
023600080430     d ds_11         e ds                  inz  extname(FISP3011ds)
023700080430     d                                          prefix(P : 3)
023800110202       // ·?12ª riga?
023900080430     d ds_12         e ds                  inz  extname(FISP3012ds)
024000080430     d                                          prefix(P : 3)
024100110202       // ·?13ª riga?
024200080430     d ds_13         e ds                  inz  extname(FISP3013ds)
024300080430     d                                          prefix(P : 3)
024400110202       // ·?14ª riga?
024500080430     d ds_14         e ds                  inz  extname(FISP3014ds)
024600080430     d                                          prefix(P : 3)
024700130205     d ds_140        e ds                  inz  extname(FISP30140s)
024800130205     d                                          prefix(P : 3)
024900110202       // ·?15ª riga?
025000080430     d ds_15         e ds                  inz  extname(FISP3015ds)
025100080430     d                                          prefix(P : 3)
025200110202       // ·?16ª riga?
025300080430     d ds_16         e ds                  inz  extname(FISP3016ds)
025400080430     d                                          prefix(P : 3)
025500110202       // ·?17ª riga?
025600080430     d ds_17         e ds                  inz  extname(FISP3017ds)
025700080430     d                                          prefix(P : 3)
025800110202       // ·?18ª riga?
025900080430     d ds_18         e ds                  inz  extname(FISP3018ds)
026000080430     d                                          prefix(P : 3)
026100130205     d ds_180        e ds                  inz  extname(FISP30180s)
026200130205     d                                          prefix(P : 3)
026300110202       // ·?19ª riga?
026400080430     d ds_19         e ds                  inz  extname(FISP3019ds)
026500080430     d                                          prefix(P : 3)
026600110202       // ·?20ª riga?
026700080430     d ds_20         e ds                  inz  extname(FISP3020ds)
026800080430     d                                          prefix(P : 3)
026900110202       // ·?21ª riga?
027000080430     d ds_21         e ds                  inz  extname(FISP3021ds)
027100080430     d                                          prefix(P : 3)
027200110202       // ·?22ª riga?
027300080430     d ds_22         e ds                  inz  extname(FISP3022ds)
027400080430     d                                          prefix(P : 3)
027500110202       // ·?23ª riga?
027600080430     d ds_23         e ds                  inz  extname(FISP3023ds)
027700080430     d                                          prefix(P : 3)
027800110202       // ·?24ª riga?
027900080430     d ds_24         e ds                  inz  extname(FISP3024ds)
028000080430     d                                          prefix(P : 3)
028100110202       // ·?25ª riga?
028200080430     d ds_25         e ds                  inz  extname(FISP3025ds)
028300080430     d                                          prefix(P : 3)
028400110202       // ·?26ª riga (vuota)?
028500110202       // ·?27ª riga?
028600080430     d ds_27         e ds                  inz  extname(FISP3027ds)
028700080430     d                                          prefix(P : 3)
028800110202       // ·?28ª riga?
028900080430     d ds_28         e ds                  inz  extname(FISP3028ds)
029000080430     d                                          prefix(P : 3)
029100110202       // ·?29ª riga?
029200081021     d ds_29         e ds                  inz  extname(FISP3029ds)
029300081021     d                                          prefix(P : 3)
029400110202       // ·?30ª riga?
029500081021     d ds_30         e ds                  inz  extname(FISP3030ds)
029600081021     d                                          prefix(P : 3)
029700110215       // ·?31ª riga?
029800110215     d ds_31         e ds                  inz  extname(FISP3031ds)
029900110215     d                                          prefix(P : 3)
030000110202       // ·?32ª riga?
030100081021     d ds_32         e ds                  inz  extname(FISP3032ds)
030200081021     d                                          prefix(P : 3)
030300110202       // ·?33ª riga?
030400081021     d ds_33         e ds                  inz  extname(FISP3033ds)
030500081021     d                                          prefix(P : 3)
030600110202       // ·?34ª riga?
030700081021     d ds_34         e ds                  inz  extname(FISP3034ds)
030800081021     d                                          prefix(P : 3)
030900110202       // ·?35ª riga (vuota)?
031000110202       // ·?36ª riga (vuota)?
031100110202       // ·?37ª riga e successive?
031200081023     d ds_37         e ds                  inz  extname(FISP3037ds)
031300081021     d                                          prefix(P : 3)
031400110202       // ·?Righe x FedEx?
031500081030     d ds_Fed        e ds                  inz  extname(FISP304Fds)
031600081030     d                                          prefix(P : 3)
031700130411
031800130411       // -?Flag operativi in TITAS30C?
031900130411     d dTASflo       e ds                  inz     qualified
032000120702
032100130214       // -?Dati aggiuntivi in FIILV00F?
032200120702     d dILVlibero    e ds                  inz     qualified
032300101220
032400101220       // -?Parametri per conversione importi in Euro?
032500061107     d YEURCOds      e ds                  inz
032600110202
032700101220       // -?Dati da AZORG00F?
032800070424     d OG143ccm      e ds                  inz     extname(OG143)
032900070424     d                                             prefix(§CM:3)
033000070424     d OG146arr      e ds                  inz     extname(OG146)
033100070424     d                                             prefix(A)
033200070424     d OG146cf£6     e ds                  inz     extname(OG146)
033300070424     d                                             prefix(£6:1)
033400070424     d OG148arr      e ds                  inz     extname(OG148)
033500070424     d                                             prefix(A)
033600070424     d OG148cf£6     e ds                  inz     extname(OG148)
033700070424     d                                             prefix(£6:1)
033800070424     d OG150arr      e ds                  inz     extname(OG150)
033900070424     d                                             prefix(A)
034000110202
034100101220       // -?Dati da FIAR400F?
034200061108     d dsBL4A        e ds                  inz
034300171117     d*// dsBL4I        e ds                  inz
034400071015     d dsBL4M        e ds                  inz
034500121018     d dTA4Q         e ds                  inz
034600110228
034700110228       // -?Dati da FIDST00F?
034800110228     d dDSTflr       e ds                  inz
034900110202
035000101220       // -?Tabelle?
035100131206     d ds$2          e ds
035200061107     d ds1A          e ds                  inz
035300061107     d ds1P          e ds                  inz
035400070306     d ds2A          e ds                  inz
035500061107     d ds3A          e ds                  inz
035600061107     d ds5E          e ds                  inz
035700061107     d dsCC          e ds
035800061107     d dsCV          e ds                  inz
035900061107     d dsEI          e ds                  inz
036000061107     d dsTB          e ds                  inz
036100061107      *
036200030515     d TIBS02ds      e ds                  inz
036300040607     d   T02mod      e                     inz('C')
036400030805     d dGED          e ds
036500130206     d dILV          e ds
036600061031     d dVPOdeco      e ds                  inz
036700031124     d dAR5gen       e ds                  inz
036800061030     d dAR5bnb       e ds                  inz
036900170517     d dCLI          e ds                  inz
037000080724
037100131210       // -?Flag operativi di FNEVB - campo EVBNOT?
037200131206     d dEVB01        e ds                  inz     qualified
037300101220       // -?Flag operativi di FNORM - campo ORMFLO?
037400100517     d dORM01        e ds                  inz
037500101220       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
037600030515     d TRUL21ds      e ds                  inz
037700120702     d   I21tla      e                     inz('L')
037800101220       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
037900070212     d FNLV55ds      e ds                  inz
038000120702     d   D55tpt      e                     inz('6')
038100101220       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
038200051129     d FNLV80ds      e ds                  inz
038300120702     d   ILV80tla    e                     inz('C')
038400101220       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
038500090619     d TNSD99ds      e ds                  inz
038600120702     d   D98tla      e                     inz('C')
038700130227
038800130227       // -?Tab. "MRA" = Bart-Mailing?
038900130227     d dMRAdan       e ds                  inz
039000130227     d   §MRAddes    e                     inz('PDA-Oltre 100 log "01"+
039100130227     d                                             ')
039200130227     d   §MRAdreg    e                     inz('COR')
039300130227     d   §MRAdmitt   e                     inz(c_Mittente)
039400130227     d   §MRAddest   e                     inz('stefano.merighi+
039500130227     d                                          @brt.it;')
039600130227     d   §MRAdidpro  e                     inz('1')
039700130227     d   §MRAdoutqi  e                     inz('EMAILIN')
039800130227
039900130227       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
040000130227     d TRTCM1ds      e ds                  inz
040100130716         //?·§CM1mitt = Indirizzo e-mail del mittente?
040200130227     d   §CM1mitt    e                     inz(c_Mittente)
040300130716         //?·§CM1dst  = Indirizzo e-mail del destinatario?
040400130227     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
040500130716         //?·§CM1tips = Tipo lettera via e-mail:?
040600130716         // ?"LET" = testo allegato in corpo con logo?
040700130716         //         ?(richiede righe libere iniziali per il logo)?
040800130716         // ?"COR" = testo integrato senza logo?
040900130716         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
041000130716         // ?"SP1" = spool allegato (in formato PDF)?
041100130227     d   §CM1tips    e                     inz('COR')
041200130716         //?·§CM1po   = Sede?
041300130227     d   §CM1po      e                     inz(c_Sede)
041400130716         //?·§CM1var  = Oggetto e-mail?
041500130227     d   §CM1var     e                     inz('*OBJM*+
041600130227     d                                     Sped. elaborata oltre 100 +
041700130227     d                                     volte in FIILV - vedi log +
041800130227     d                                     "01"')
041900130716         //?·§CM1sts  = Stato?
042000130227     d   §CM1sts     e                     inz(*off)
042100130716         //?·§CM1idp  = Id processo?
042200130227     d   §CM1idp     e                     inz('2')
042300101220
042400101220       // -?Status ds?
042500030515     d Status         sds
042600120702     d   SDSpgm          *proc
042700120702     d   SDSprm          *parms
042800120702     d   SDSusr              254    263
042900101220
043000101220       // -?Parametro Numero Distinta?
043100101220     d ds_Distinta     ds             9    inz  qualified
043200101220     d   P_Fgs                        3s 0 inz
043300101220     d   P_Ndc                        6s 0 inz
043400080430
043500101207       // -?Parametro Numero Spedizione?
043600110207     d ds_Spedizione   ds            16    inz  qualified
043700080430     d   P_Aas                        4s 0 inz
043800080430     d   P_Lnp                        3s 0 inz
043900080430     d   P_Nrs                        2s 0 inz
044000080430     d   P_Nsp                        7s 0 inz
044100101220
044200080519     d BARcodeDS       ds            17    inz
044300120702     d   BARcdeAAS                    2s 0 inz
044400120702     d   BARcdeLNP                    3s 0 inz
044500120702     d   BARcdeNRS                    2s 0 inz
044600120702     d   BARcdeNSP                    7s 0 inz
044700101220
044800030515     d KEYpar          ds             8    inz
044900120702     d   WTPpar                1      1    inz
045000120702     d   WCDpar                2      3    inz
045100120702     d   Wprogr                4      8    inz('S0001')
045200101220
045300140303     d Elem_CP         ds                  inz
045400120702     d   WCPdcm                       8s 0 inz
045500140303     d   WCPocm                       4s 0 inz
045600120702     d   WCPncl                       5s 0 inz
045700140303     d   WCPfir                            inz  like(EVB2dati)
045800101213
045900101213       //--------------------------------------------------------------
046000101213       //?Definizione variabili globali.                               ?
046100101213       //--------------------------------------------------------------
046200101213
046300101220       // -?Parametri?
046400080430     d Spedizione      s             16
046500101217     d Distinta        s              9
046600101217     d DataOraCrea     s             14
046700080430     d Risultato       s              2
046800080715     d RootFolder      s            100
046900080519     d FileName        s             30
047000080731     d TemplateName    s             20
047100110201     d LastRow         s            120
047200131016     d RtnEsito        s             10i 0                                      // -?NON usato?
047300131016     d RtnOpCode       s             10                                         // -?NON usato?
047400131016     d RtnStatus       s             10i 0                                      // =?SQLcode?
047500080430     d P_Prt           s              1
047600101220
047700130214       // -?Flags booleani?
047800101220     d $Mamma          s               n   inz
047900130215     d $Mamma_FedEx    s               n   inz
048000101220     d $FIAR8          s               n   inz
048100130411     d $§PDF           s               n   inz
048200110204     d $FIILV          s               n   inz
048300101220     d $NoDesPartic    s               n   inz
048400110117     d*// $BolloVirt      s               n   inz(*off)
048500120113     d $No_x_Cons      s               n   inz
048600101220     d $ConsParz       s               n   inz
048700101220     d $CAeur          s               n   inz
048800100517     d $PreavvORM      s               n   inz
048900101220     d $GeoAtt         s               n   inz
049000131206     d $1o_Passaggio   s               n   inz
049100101220
049200101220       // -?Indici di schiera?
049300080523     d XX              s              3  0 inz
049400140304     d YY              s              3  0 inz
049500080523     d WX              s              3  0 inz
049600130313
049700130313       // -?Nome esteso Libreria/File del file TITAS30C?
049800130313     d wLibFileTITAS   s             21a   inz
049900080724
050000101207       // -?Campi di stampa?
050100130227     d P_Fgs2          s                   inz  like(ds_Distinta.P_Fgs)
050200130227     d P_Ndc2          s                   inz  like(ds_Distinta.P_Ndc)
050300101220
050400101220       // -?Campi di comodo alfanumerici?
050500030717     d w_CoOr          s             35    inz
050600081030     d wAWB            s             12    inz
050700030515     d w001a           s              1    inz
050800030515     d w008a           s              8    inz
050900030515     d w015a           s             15    inz
051000100517     d w053a           s             53    inz
051100070424     d wDateA          s              8    inz
051200070717     d wGeoTel         s              1    inz
051300070717     d wGeoCon         s              1    inz
051400080718     d wSysName        s              8    inz
051500130411      *? x TEST ?
051600130411     d $Test           s              3    inz
051700101220
051800101220       // -?Campi di comodo numerici?
051900030515     d w0080           s              8  0 inz
052000030624     d w0112           s             11  2 inz
052100030515     d w0140           s             14  0 inz
052200071105     d wDate           s              8  0 inz
052300090120     d w_Row           s              3  0 inz(c_Row)
052400110309     d wCountRec01     s              5  0 inz
052500101220
052600131008       // -?Campi utilizzati per impostazione dell'output?
052700110117     d*// wBolloVirt      s              4  2 inz
052800030515     d Wcas            s             13  3 inz
052900030515     d Wiva            s             11  2 inz
053000071015     d Wpdc4A          s              4    inz
053100030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
053200090619     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
053300101220
053400101220       // -?Campi utilizzati per il salvataggio momentaneo di dati?
053500040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
053600121018     d wB0cfi          s                   like(DB0cpi)   inz
053700121018     d wB0piv          s                   like(DB0cpi)   inz
053800120702     d wB0nrs          s                   like(ARBnrs)   inz
053900070514     d wB0ntc          s                   like(ARBntc)   inz
054000080523     d wB0dcm          s                   like(ARBdcm)   inz
054100070514     d wB0dcp          s                   like(ARBdcp)   inz
054200081120     d wB0tmc          s                   like(ARBtmc)   inz
054300130205     d wB0fiv          s                   like(AR6fiv)   inz
054400080506     d Wnsc            s                   like(ARTnsc)   inz
054500080506     d Wtrc            s                   like(AR4trc)   inz
054600030515     d Wctr            s                   like(DB0ctr)   inz
054700031124     d Wdsp            s                   like(DB0dft)   inz
054800090619     d WddtCLN         s                   like(DB0drt)   inz
054900090619     d WdspCLN         s              6  0                inz
055000030515     d Wsvx            s                   like(AR6sv1)   inz
055100030515     d Wvax            s                   like(AR6va1)   inz
055200110203     d wCod_Log        s                   like(ILVcol)   inz
055300090619
055400140303       //--------------------------------------------------------------
055500140303       //?Definizione procedure usate.                                 ?
055600090619       //--------------------------------------------------------------
055700100322
055800100322       // -?Procedure name: getCurrentSystemName?
055900100322       //  ?Purpose:        Reperisce l'attributo di rete current System Name?
056000100322       //  ?Returns:        Current System Name?
056100130716       //  ?(vedi "QWCRNETA")?
056200100322     d getCurrentSystemName...
056300100322     d                 pr             8a
056400100322
056500100322       //--------------------------------------------------------------
056600140303       //?Definizione procedure esterne.                               ?
056700100322       //--------------------------------------------------------------
056800100322
056900110201       // -?Calcolo check digit del barcode?
057000110201     d TRUL28ds      e ds                  inz
057100110201     d  I28tla       e                     inz('L')
057200110201     d  I28mod       e                     inz('BAR')
057300110201     d TrUl28r1        pr                  extpgm('TRUL28R1')
057400110201     d  trul28ds                           likeds(TRUL28ds)
057500110201
057600100322       // -?Calcolo tempi di delivery?
057700100322      /copy gaitrasrc/srcProtoPR,TNSD99R
057800130206
057900130206       // -?Ricerca/Controllo tabelle?
058000130206      /copy gaitrasrc/srcProtoPR,TIBS02R
058100100322
058200110201       // -?Parametri per API "QWCRNETA"?
058300110201      /copy QSYSINC/QRPGLESRC,QWCRNETA
058400110201     d qWcrNetA_dataReturned...
058500110201     d                 ds                  qualified inz
058600110201     d  qWcRDR                             likeds(qWCRDR)
058700110201     d  qWcOAT                       10i 0 dim(00001)
058800110201     d  qWcARRAY                     24
058900110201     d qWCRNETA_networkAttributeInformationTable...
059000110201     d                 ds                  qualified inz
059100110201     d  qWcRAT                             likeds(qWCRAT)
059200110201     d  qWcDATA                       8
059300100322       // -?Reperimento attributo di rete current System Name?
059400100322       //  ?(vedi "getCurrentSystemName")?
059500100322      /copy gaitrasrc/srcProtoPR,QWCRNETA
059600130227
059700130227       // -?Parametri API QCAPCMD (Process Commands)?
059800130227     d Qcmd            s           2048    inz  varying
059900130227      /copy qSysInc/qRpgleSrc,QCAPCMD
060000130227       // -?API QCAPCMD (Process Commands)?
060100130227      /copy gaitrasrc/srcProtoPR,QCAPCMD
060200130227
060300130227       // -?Parametri gestione errori API.?
060400130227      /copy qsysinc/qRpgleSrc,QUSEC
060500080710
060600080710       //--------------------------------------------------------------
060700140303       //?Definizione key-list.                                        ?
060800080710       //--------------------------------------------------------------
060900130313
061000130422       // -?File FIIRD11L?
061100130422     d k01fiird11    e ds                  extname(FIIRD11L : *key)
061200130313     d                                     prefix(k_)
061300130313     d                                     inz
061400130313
061500130313       // -?File TITAS30C?
061600130313     d k05titas30    e ds                  extname(TITAS30C : *key)
061700130313     d                                     prefix(k_)
061800130313     d                                     inz
061900131206
062000131206       // -?File FNEVB04L?
062100131206     d keyFNEVB04    e ds                  extname(FNEVB04L : *key)
062200131206     d                                     prefix(k_)
062300131206     d                                     inz
062400140303
062500140303       // -?File FNEVB22L?
062600140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
062700140303     d                                     prefix(k_)
062800140303     d                                     inz
062900170517
063000170517       // -?File TFNTC01L?
063100170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
063200170517     d                                     prefix(k_)
063300170517     d                                     inz
063400080710
063500101207       // -?File FIDST01L?
063600110228     d k03fidst01    e ds                  extname(FIDST01L : *key)
063700080710     d                                     prefix(k_)
063800080710     d                                     inz
063900090216
064000110202       // -?File FIILV01L?
064100110202     d k06fiilv01    e ds                  extname(FIILV01L : *key)
064200110207     d                                     prefix(k_)
064300090216     d                                     inz
064400030515
064500101220       //==============================================================
064600101220       //?RIEPILOGO INDICATORI                                         ?
064700101220       //--------------------------------------------------------------
064800101220       // 31    - di comodo
064900101220       // 35    - Porto Franco: segnala
065000101220       //         "PER RICEVUTA DELLA MERCE SENZA RISERVE"
065100101220       //?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia
065200101220       //         e indica "TOTALE DA ESIGERE AL RITIRO"
065300120524       // 70    - Trattasi di BOLLA BRT
065400101220       // 71    - Trattasi di BOLLA DPD
065500101220       // 72    - Trattasi di BOLLA FedEx
065600101220       // 73    - Trattasi di BOLLA EuroExpress
065700101220       //==============================================================
065800030515
065900101213       //--------------------------------------------------------------
066000101213       //?M A I N - L I N E                                            ?
066100101213       //--------------------------------------------------------------
066200101213
066300030515     c     *Entry        plist
066400080430     c                   parm                    Spedizione
066500101217     c                   parm                    Distinta
066600101217     c                   parm                    DataOraCrea
066700080430     c                   parm                    Risultato
066800080715     c                   parm                    RootFolder
066900080519     c                   parm                    FileName
067000080731     c                   parm                    TemplateName
067100110201     c                   parm                    LastRow
067200080430     c                   parm                    RtnEsito
067300080430     c                   parm                    RtnOpCode
067400080430     c                   parm                    RtnStatus
067500080430     c                   parm                    P_Prt
067600080430      *
067700110519      *?P_Prt:?non passato se richiamato da server, serve solo se?
067800110519      *       ?chiamato da menù AS400 x test?
067900101221      *   Valori: qualsiasi = richiamato da pgm. per test (stampa)
068000080430      *
068100070123      *
068200130924      * Operazioni iniziali
068300131016       //?SE rilevati errori nei parametri di input =>?
068400131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
068500070123     c                   exsr      sr_Start
068600030910      *
068700130924      * Pulizia dei campi di output e/o di lavoro
068800130924     c                   exsr      sr_ClrFLD
068900030515      *
069000130924      * Caricamento dei dati per l'immagine della LdV
069100131016       //?SE rileva errori nella distinta o nella spedizione =>?
069200131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
069300110208     c                   exsr      sr_CarARB
069400030515      *
069500130924      * Controllo del tipo LDV di cui generare l'immagine
069600110208     c                   exsr      sr_TipoLDV
069700030515      *
069800080430      * Impostazione dei campi di output con quelli della DS
069900110208     c                   exsr      sr_PrepPRT
070000030515      *
070100080430      * Decodifica dei dati della LDV
070200110208     c                   exsr      sr_DecodLDV
070300080806      *
070400080806      * Impostazione dati per immagine LdV
070500131016     c                   exsr      sr_DatiOUT
070600030515      *
070700030515      * Chiusura pgm
070800080430     c                   exsr      sr_End
070900030515      *
071000030515      *---------------------------------------------------------------*
071100050217      *?OPERAZIONI INIZIALI                                          ?*
071200030515      *---------------------------------------------------------------*
071300030515     c     *InzSR        BEGSR
071400070424      *
071500070424      * Reperisco la data del giorno
071600070424     c                   time                    w0140
071700070424     c                   move      w0140         w0080
071800070424     c     *eur          movel     w0080         Data_EUR
071900070424     c     *iso          movel     Data_EUR      w0080
072000070424     c                   move      w0080         wDateA
072100071105     c                   eval      wDate  = w0080
072200030515      *
072300030515      * Imposto i campi chiave "fissi"
072400040223     c                   eval      TBLkut = 1
072500100901     c                   eval      ARStrc = 'C'
072600030515      *
072700040617      * Impostazione dati in KPJBA
072800070308     c                   eval      KNMUS  = SDSusr
072900030515      *
073000030515      * Reperisco la moneta di conto
073100030515     c                   clear                   dGED
073200040607     c                   clear                   TIBS02ds
073300040607     c                   eval      T02mod = 'C'
073400040607     c                   eval      T02cod = 'GED'
073500040607     c                   eval      T02ke1 = '1         '
073600030515     c                   call      'TIBS02R'
073700030515     c                   parm                    KPJBA
073800040607     c                   parm                    TIBS02ds
073900030515if  1c                   if        T02err = *blanks
074000030515     c                   movel     T02uni        dGED
074100030515e   1c                   endif
074200061031      *
074300131008      * Reperisco data di decorrenza indicaz. "Per ricevuta di ...
074400131008      *                                        senza riserve"
074500131008      *              e di decorrenza indicaz. "Consegna al mattino"
074600061031     c                   clear                   dVPOdeco
074700061031     c                   clear                   TIBS02ds
074800061031     c                   eval      T02mod = 'C'
074900071112     c                   eval      T02cod = 'VPO'
075000061031     c                   eval      T02ke1 = 'DECO      '
075100061031     c                   call      'TIBS02R'
075200061031     c                   parm                    KPJBA
075300061031     c                   parm                    TIBS02ds
075400061031if  1c                   if        T02err = *blanks
075500061031     c                   movel     T02uni        dVPOdeco
075600061031e   1c                   endif
075700030515      *
075800030515     c                   ENDSR
075900070123      *
076000101220      /free
076100101220
076200101220       //--------------------------------------------------------------
076300101220       //?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?
076400101220       //--------------------------------------------------------------
076500101220       BEGSR  sr_Start;
076600101220
076700101220         exec sql  set option  dynusrprf = *owner,
076800101220                               closqlcsr = *endmod;
076900101220
077000101221         *inLR = *on;
077100080509
077200110517         // -?Impostazione iniziale dei dati di ritorno?
077300080509         Risultato = 'OK';
077400110202         clear  RootFolder;
077500110202         clear  FileName;
077600120524         //TemplateName = 'ldvbase00 ';   //?Standard - BARTOLINI senza box aggiuntivo?
077700120524         //TemplateName = 'ldvbase10 ';   //?Standard - BARTOLINI-BRT senza box aggiuntivo?
077800130924         TemplateName = 'ldvbase20 ';     //?Standard - BRT senza box aggiuntivo?
077900110202         clear  LastRow;
078000110202         clear  RtnEsito;
078100110202         clear  RtnOpCode;
078200110202         clear  RtnStatus;
078300110207
078400110207         // -?Apertura archivio di output - comunque utilizzato?
078500110207         if  NOT  %open(FIILV01L);
078600110207           open(e)  FIILV01L;
078700110207           if  %error;
078800110207             exsr  sr_ErrOpenFile;
078900110207           endif;
079000110207         endif;
079100110207
079200110202         clear  k06fiilv01;
079300110203         clear  FIILV000;
079400080509
079500110203         // »? STEP 0 ?«
079600131016         // -?Controllo parametri ricevuti.  SE rileva errori =>?
079700131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
079800130716         exsr  sr_STEP_0_CtrlParms;
079900110203
080000080430
080100101213         // -?Impostazione spedizione ricevuta come parametro?
080200080430         ds_Spedizione = Spedizione;
080300110126         // -?Impostazione distinta ricevuta come parametro?
080400110126         ds_Distinta = Distinta;
080500080430
080600101213         // -?Impostazione iniziale dei dati nella ds FNLSB5DS?
080700130924         //clear FNLSB5ds;            ?(già così)?
080800080430         DB0tbo = 'A';
080900110202         DB0aas = ds_Spedizione.P_Aas;
081000110202         DB0lnp = ds_Spedizione.P_Lnp;
081100110202         DB0nrs = ds_Spedizione.P_Nrs;
081200110202         DB0nsp = ds_Spedizione.P_Nsp;
081300080519
081400101213         // -?Impostazione campo di output "identificativo spedizione"?
081500080519         clear  BarCodeDS;
081600080519         BARcdeAAS = %int( %subst( %editc( DB0aas : 'X' ) : 3 : 2) );
081700080519         BARcdeLNP = DB0lnp;
081800080519         BARcdeNRS = DB0nrs;
081900080519         BARcdeNSP = DB0nsp;
082000080519         FileName  = BarCodeDS;
082100130313
082200130716         // -?Impostazione chiave di accesso al file FIIRD11L?
082300130313         k_IRDfgsndc = Distinta;
082400130313
082500130313         // -?Impostazione chiave di accesso al file TITAS30C?
082600130313         clear  k05titas30;
082700130313         k_TASaas = DB0aas;
082800130313         k_TASlnp = DB0lnp;
082900130313         k_TASnrs = DB0nrs;
083000130313         k_TASnsp = DB0nsp;
083100131206
083200131206         // -?Impostazione chiave di accesso al file FNEVB04L?
083300131206         clear  keyFNEVB04;
083400131206         k_EVBaas = DB0aas;
083500131206         k_EVBlnp = DB0lnp;
083600131206         k_EVBnrs = DB0nrs;
083700131206         k_EVBnsp = DB0nsp;
083800131210         //k_EVBcev = 'MIC';          ?(leggere TUTTI i rec.)?
083900140303
084000140303         // -?Impostazione chiave di accesso al file FNEVB22L?
084100140303         clear  keyFNEVB22;
084200140303         k_EVB2aas = DB0aas;
084300140303         k_EVB2lnp = DB0lnp;
084400140303         k_EVB2nrs = DB0nrs;
084500140303         k_EVB2nsp = DB0nsp;
084600140303         //k_EVB2dtv = .....;
084700140303         //k_EVB2orv = .....;
084800140303         //k_EVB2cev = 'P  ';
084900110204
085000110204         // -?Impostazione chiave di accesso al file FIILV01L?
085100110204         k_ILVaas = DB0aas;
085200110204         k_ILVlnp = DB0lnp;
085300110204         k_ILVnrs = DB0nrs;
085400110204         k_ILVnsp = DB0nsp;
085500110225         k_ILVfgs = ds_Distinta.p_Fgs;
085600110225         k_ILVndc = ds_Distinta.p_Ndc;
085700120702         clear  dILVlibero;
085800130411
085900130411
086000130411         IF  '1' <> '1';
086100130411
086200130924           // -? CHIODI MOMENTANEI PER TEST                                 ?
086300130924           //  ? da attivare per ricavare immagini di esempio forzando sped ?
086400130924           clear  $Test;
086500130411           Select;
086600130411             // -?Spedizione FedEx?
086700130411             When  Spedizione = '2011050001843581' and Distinta = '145040335';
086800130411               $Test = 'FED';
086900130411             // -?Spedizione DPD?
087000130411             When  Spedizione = '2011190000579210' and Distinta = '145040339';
087100130411               $Test = 'DPD';
087200130411             // -?Spedizione con Consegna Parziale Precedente?
087300130411             When  Spedizione = '2011001000816011' and Distinta = '145040361';
087400130411               $Test = 'C_P';
087500130411             // -?Spedizione Normale?
087600130411             When  Spedizione = '2011163000042281' and Distinta = '145040333';
087700130411               $Test = 'NOR';
087800130411             // -?Altra spedizione: NON ELABORABILE?
087900130411             Other;
088000130411               Risultato = 'NO';
088100130924               //RtnOpCode = 'NO_IN_TEST';
088200130924               //RtnEsito  = -1;
088300130924               // -?Uscita dalla s.p.?
088400130411               exsr  sr_End;
088500130411           EndSl;
088600130411
088700130411         ENDIF;
088800130411
088900080430
089000110208         // -?Apertura archivi di input - comunque utilizzati?
089100110207         if  NOT  %open(TABEL00F);
089200110207           open(e)  TABEL00F;
089300110207           if  %error;
089400110207             exsr  sr_ErrOpenFile;
089500110207           endif;
089600110207         endif;
089700110207
089800110207         if  NOT  %open(AZORG01L);
089900110207           open(e)  AZORG01L;
090000110207           if  %error;
090100110207             exsr  sr_ErrOpenFile;
090200110207           endif;
090300110207         endif;
090400110207
090500110207         if  NOT  %open(FNARB01L);
090600110207           open(e)  FNARB01L;
090700110207           if  %error;
090800110207             exsr  sr_ErrOpenFile;
090900110207           endif;
091000110207         endif;
091100110207         if  NOT  %open(FIAR601L);
091200110207           open(e)  FIAR601L;
091300110207           if  %error;
091400110207             exsr  sr_ErrOpenFile;
091500110207           endif;
091600110207         endif;
091700110207         if  NOT  %open(FIAR701L);
091800110207           open(e)  FIAR701L;
091900110207           if  %error;
092000110207             exsr  sr_ErrOpenFile;
092100110207           endif;
092200110207         endif;
092300110207         if  NOT  %open(FIAR901L);
092400110207           open(e)  FIAR901L;
092500110207           if  %error;
092600110207             exsr  sr_ErrOpenFile;
092700110207           endif;
092800110207         endif;
092900110207         if  NOT  %open(FIDST01L);
093000110207           open(e)  FIDST01L;
093100110207           if  %error;
093200110207             exsr  sr_ErrOpenFile;
093300110207           endif;
093400110207         endif;
093500101213
093600101213         if  NOT  %open(FIAR401L);
093700101213           open(e)  FIAR401L;
093800101213           if  %error;
093900101213             exsr  sr_ErrOpenFile;
094000101213           endif;
094100101213         endif;
094200101213         if  NOT  %open(FIAR501L);
094300101213           open(e)  FIAR501L;
094400101213           if  %error;
094500101213             exsr  sr_ErrOpenFile;
094600130411           endif;
094700130411         endif;
094800101213
094900101220       ENDSR;
095000101220
095100101213       //--------------------------------------------------------------
095200101213       //?Restituzione errore in fase di apertura di un file           ?
095300101213       //--------------------------------------------------------------
095400101213       BEGSR  sr_ErrOpenFile;
095500101213
095600101213         Risultato = 'NO';
095700130924         //RtnOpCode = 'ERR_OPEN  ';
095800101213         //RtnEsito  = -1;
095900101213
096000130924         // -?Uscita dalla s.p.?
096100101213         exsr  sr_End;
096200101213
096300101213       ENDSR;
096400130716
096500130716       //--------------------------------------------------------------
096600130716       //?»  STEP 0  «                                                 ?
096700130716       //?Controllo dei parametri ricevuti, reperimento nome del       ?
096800130716       //?sistema e dei percorsi per le immagini in base ad esso.      ?
096900130716       //--------------------------------------------------------------
097000130716       BEGSR  sr_STEP_0_CtrlParms;
097100130716
097200130716         //   _________________________________________________________
097300130716         // -? Controllo lunghezza e numericità dei parametri ricevuti ?
097400130716         //   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
097500130716         SELECT;
097600130716
097700130716           // -?NUMERO SPEDIZIONE?
097800130716           WHEN  ( %len( %trimr( Spedizione ) ) )  <> 16     or
097900130716                 ( %check (c_Digits : Spedizione) ) > *zero  or
098000130716                                      Spedizione   <= *zero;
098100130716             Risultato = 'XX';
098200130924             //RtnOpCode = 'ERR_NUMSPE';
098300130716             //RtnEsito  = -1;
098400130716             // -?Registrazione log in FIILV?
098500130716             wCod_Log = '21';
098600130716             exsr  sr_WrtFIILV;
098700130716             // -?Uscita dalla s.p.?
098800130716             exsr  sr_End;
098900130716
099000130716           // -?NUMERO DISTINTA?
099100130716           WHEN  ( %len( %trimr( Distinta ) ) )  <> 9      or
099200130716                 ( %check (c_Digits : Distinta) ) > *zero  or
099300130716                                      Distinta   <= *zero;
099400130716             Risultato = 'XX';
099500130924             //RtnOpCode = 'ERR_DISTIN';
099600130716             //RtnEsito  = -1;
099700130716             // -?Registrazione log in FIILV?
099800130716             wCod_Log = '21';
099900130716             exsr  sr_WrtFIILV;
100000130716             // -?Uscita dalla s.p.?
100100130716             exsr  sr_End;
100200130716         ENDSL;
100300130716
100400130716         // ________________________________________________________
100500130716         //? Reperimento nome del sistema per impostazione percorso ?
100600130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
100700130716         wSysName = getCurrentSystemName();
100800130716         if  wSysName = *blank;
100900130716           Risultato  = 'NO';
101000130924           //RtnOpCode  = 'SYS_ERR   ';
101100130924           //RtnEsito   = -1;
101200130924           // -?Uscita dalla s.p.?
101300130716           exsr  sr_End;
101400130716         endif;
101500130716
101600130716         // ______________________________________________________
101700130716         //? Reperimento dei percorsi in base al nome del sistema ?
101800130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
101900130716         clear  dILV;
102000130716         reset  TIBS02ds;
102100130716         T02cod = 'ILV';
102200130716         T02ke1 = %subst( wSysName : 1 : 6 );
102300130716         T02ke2 = 'LINEA';
102400130716         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
102500130716         if  T02err = *blank;
102600130716           dILV = T02uni;
102700130716         endif;
102800130716
102900130716       ENDSR;
103000110208
103100110208       //--------------------------------------------------------------
103200130924       //?Pulizia dei campi di output e/o di lavoro                    ?
103300110208       //--------------------------------------------------------------
103400130924       BEGSR  sr_ClrFLD;
103500080430
103600081021         clear ds_01;
103700101213       //clear ds_02;                      ?(riga vuota)?
103800080430         clear ds_03;
103900080430         clear ds_04;
104000080430         clear ds_05;
104100101213       //clear ds_06;                      ?(riga vuota)?
104200080430         clear ds_07;
104300101213       //clear ds_08;                      ?(riga vuota)?
104400080430         clear ds_09;
104500080430         clear ds_10;
104600080430         clear ds_11;
104700080430         clear ds_12;
104800080430         clear ds_13;
104900080430         clear ds_14;
105000130205         clear ds_140;
105100080430         clear ds_15;
105200080430         clear ds_16;
105300080430         clear ds_17;
105400080430         clear ds_18;
105500130205         clear ds_180;
105600080430         clear ds_19;
105700080430         clear ds_20;
105800080430         clear ds_21;
105900080430         clear ds_22;
106000080430         clear ds_23;
106100080430         clear ds_24;
106200080430         clear ds_25;
106300101213       //clear ds_26;                      ?(vuoto)?
106400080430         clear ds_27;
106500080430         clear ds_28;
106600081021         clear ds_29;
106700081021         clear ds_30;
106800110215         clear ds_31;
106900081021         clear ds_32;
107000081021         clear ds_33;
107100081021         clear ds_34;
107200081023         clear ds_37;
107300081030         clear ds_Fed;
107400080430
107500080430      /end-free
107600030515      *
107700060524     c                   clear                   dsBL4A
107800171117     c*//                clear                   dsBL4I
107900110204     c                   clear                   dsBL4M
108000100517     c                   clear                   dOrm01
108100041207      *
108200070514     c                   reset                   $Mamma
108300130215     c                   reset                   $Mamma_FedEx
108400060921     c                   reset                   $FIAR8
108500130411     c                   reset                   $§PDF
108600110204     c                   reset                   $FIILV
108700030710     c                   reset                   $NoDesPartic
108800110117     c*//                reset                   $BolloVirt
108900040707     c                   reset                   $No_x_Cons
109000070319     c                   reset                   $ConsParz
109100030515     c                   reset                   $CAeur
109200100901     c                   reset                   $PreavvORM
109300101220     c                   reset                   $GeoAtt
109400070717     c                   clear                   wGeoTel
109500070717     c                   clear                   wGeoCon
109600070717     c                   clear                   Wdsp
109700090619     c                   clear                   WddtCLN
109800090619     c                   clear                   WdspCLN
109900030717     c                   clear                   W_CoOr
110000110117     c*//                clear                   wBolloVirt
110100030515     c                   clear                   Wcas
110200030515     c                   clear                   Wiva
110300071015     c                   clear                   Wpdc4a
110400121018     c                   clear                   wB0cfi
110500121018     c                   clear                   wB0piv
110600031015     c                   clear                   wB0nrs
110700070514     c                   clear                   wB0ntc
110800080523     c                   clear                   wB0dcm
110900070514     c                   clear                   wB0dcp
111000081120     c                   clear                   wB0tmc
111100130205     c                   clear                   wB0fiv
111200030515      *
111300030515     c                   clear                   $NP
111400070319     c                   reset                   $CP
111500070319     c                   reset                   $CPdcm
111600030515      *
111700030515     c                   movea     *zeros        *in(01)
111800030515      *
111900030515     c                   ENDSR
112000030515      *
112100030515      *---------------------------------------------------------------*
112200131008      *?Determino il modulo LDV da usare per l'immagine              ?*
112300131008      *?(se DPD, FedEx o BRT - con consegne parziali precedenti o    ?*
112400131008      *? promrmoria per l'autotrasportatore).                        ?*
112500030515      *---------------------------------------------------------------*
112600110208     c     sr_TipoLDV    BEGSR
112700070215      *
112800070212      * Impostazione della data spedizione
112900110208      * (se non già impostata in subr. "sr_CarARB")
113000070212if  1c                   if        Wdsp   =  *zeros
113100070212if  2c                   if        DB0mgs <> *zeros
113200070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
113300070212x   2c                   else
113400070212      *   prendo la data del giorno se non c'è la data spedizione
113500070212     c                   time                    w0140
113600070212     c                   move      w0140         w0080
113700070212     c     *eur          movel     w0080         Data_EUR
113800070212     c     *iso          movel     Data_EUR      Wdsp
113900070212e   2c                   endif
114000070212e   1c                   endif
114100040607      *
114200070308      * Dati della filiale del mittente
114300040607     c                   eval      WX     =  DB0ccm / 10000
114400040607if  1c                   if        WX     <> ORGfil
114500040607     c     WX            chain     AZORG
114600040607e   1c                   endif
114700040607if  1c                   if        %found(AZORG01L)
114800040607     c                   movel     ORGde3        OG143ccm
114900040607x   1c                   else
115000040607     c                   clear                   OG143ccm
115100040607e   1c                   endif
115200040311      *
115300040311      * Dati della linea di arrivo
115400131203      *?(SONO GIÀ STATI REPERITI NELLA SUBR. "SR_DECLNA",?
115500131203      *? ESEGUITA DALLA SUBR. "SR_CARARB")?
115600070212      *
115700070212      * Dati della capofila della linea di arrivo
115800131205      * - per verificare TELLUS
115900070717      *
116000070717      *** Imposta flag attivazione GEO x LNA
116100070717     c                   eval      wGeoCon = A§OGtlls
116200070717     c                   eval      wGeoTel = A§OGgeot
116300070717if  1c                   if             A§OGdda  > *zeros
116400070717     c                             and  A§OGdda  < C_DateMax_A
116500070717     c                             and  A§OGdda <= wDateA
116600101220     c                   eval      $GeoAtt = *on
116700070717e   1c                   endif
116800070717      *
116900110203if  1c                   IF             wGeoCon <> 'S'
117000070717     c                              or  wGeoTel <> 'S'
117100110203     c                              or  $GeoAtt <> *on
117200070212      * - Reperimento capofila (£6)
117300070212if  2c                   if        DB0lna <> D55lin
117400070212     c                   reset                   FNLV55ds
117500130924     c***?già così:?     eval      D55tpt =  '6'
117600070212     c                   eval      D55lin =  DB0lna
117700070212     c                   eval      D55drf =  Wdsp
117800070212     c                   call      'FNLV55R'
117900070212     c                   parm                    FNLV55ds
118000070212if  3c                   if        D55err <> *blanks
118100070212     c                   eval      D55tfa =  DB0lna
118200070212e   3c                   endif
118300070212e   2c                   endif
118400070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
118500070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
118600070212if  2c                   if        D55tfa <> ORGfil
118700070212     c     D55tfa        chain     AZORG
118800071109if  3c                   if        %found(AZORG01L)
118900070717     c                   movel     ORGde6        OG146cf£6
119000070717     c                   movel     ORGde8        OG148cf£6
119100071109x   3c                   else
119200071109     c                   clear                   OG146cf£6
119300071109     c                   clear                   OG148cf£6
119400071109e   3c                   endif
119500071109e   2c                   endif
119600070717      *** Imposta flag attivazione GEO x capofila £6
119700070717     c                   eval      wGeoCon = £6OGtlls
119800070717     c                   eval      wGeoTel = £6OGgeot
119900071109if  2c                   if             £6OGdda  > *zeros
120000070717     c                             and  £6OGdda  < C_DateMax_A
120100070717     c                             and  £6OGdda <= wDateA
120200101220     c                   eval      $GeoAtt = *on
120300071109e   2c                   endif
120400070717      *
120500070212e   1c                   ENDIF
120600040607      *
120700130411      * Reperimento dati da FIAR5 / "GEN"
120800130411      *   (per verificare se esistono Packing-List e/o PDF allegati
120900130411      *    - oltre che per reperire Nominativo e Telefono)
121000130411     c                   exsr      RepAR5gen
121100130411      *
121200130411      * Verifica se LDV con DDT e/o PDF del mittente
121300130411     c                   exsr      CercaDDToPDF
121400131206      *
121500131206      * Controllo se già effettuato il 1° passaggio c/o destinatario
121600131206     c                   exsr      sr_Check1pass
121700070319      *
121800070319      * Verifica se richiesta spedizione già parzialmente consegnata
121900070319      *   (da stampare nel box "Note per l'autotrasportatore")
122000070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
122100070717      * completamente
122200110203if  1c                   if             wB0dcp   >  *zeros
122300101220     c                             and  $GeoAtt  =  *on
122400070413     c                   eval      $ConsParz     =  *on
122500070319e   1c                   endif
122600030515      *
122700040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
122800040607      *   riserve)
122900120524      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
123000040223     c                   movea     '1000'        *in(70)
123100040311      *
123200030515sel 1c                   SELECT
123300071213      *
123400170517      * LDV DPD?(dalla fil. del mittente, NON della LNP)?
123500071213w   1c                   WHEN      §CMntw = 'DPD'
123600071213     c                   movea     '0100'        *in(70)
123700071213      *
123800071213      * LDV con mittente FedEx
123900150430w   1c                   WHEN      DB0ccm = c_CliFedEx   or
124000150430     c                             DB0ccm = c_CliFedEx2
124100071213     c                   movea     '0010'        *in(70)
124200030515      *
124300030515e   1c                   ENDSL
124400040707      *
124500040707      * Controllo se devo stampare "Non utilizzare per la consegna"
124600040707      * nella copia per il vettore,?senza?copia per il destinatario:
124700130924     c***?(già fatto:)?  reset                   $No_x_Cons
124800071203      * - LdV recapito c/assegni:
124900100322      *   stampata in arrivo e con
125000120113if  1c                   if             DB0lnp =  102
125100070306     c                             and  DB0cbo =  'FW'
125200071203     c                             and  DB0nft =  *zeros
125300071203      *   - fil. arrivo con GEO attivato
125400071203     c                             and (DB0fl1    =  'A'
125500071203     c                              or  wGeoCon   =  'S'
125600071203      *   - fil. arrivo abilitata al picking
125700120113     c                              or  A§OGpck   =  'S')
125800070306     c                   eval      $No_x_Cons  =  *on
125900120113e   1c                   endif
126000030515      *
126100030515     c                   ENDSR
126200040607      *
126300040607      *---------------------------------------------------------------*
126400130411      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
126500130411      *?               di uno o più PDF allegati da stampare         ?*
126600040607      *---------------------------------------------------------------*
126700130411     c     CercaDDToPDF  BEGSR
126800040607      *
126900130924     c***?(già fatto:)?  reset                   $FIAR8
127000130924     c***?(già fatto:)?  reset                   $§PDF
127100040607      *
127200130924     c***?(già fatto:)?  exsr      RepAR5gen
127300130411      *
127400130411      * -?Verifica esistenza PDF allegato?
127500130411     c                   eval      $§PDF  = (§AR5als = 'S')
127600130411      *
127700130411      * -?Verifica esistenza packing-list?
127800130411     c                   eval      $FIAR8 = (§AR5alx = 'S')
127900040607      *
128000080505     c                   ENDSR
128100131206      *
128200131206      /free
128300131206
128400131206       //--------------------------------------------------------------
128500131206       //?Verifica se già effettuato il 1° passaggio.                  ?
128600131206       //--------------------------------------------------------------
128700131206       BEGSR  sr_Check1pass;
128800131206
128900131206         //reset  $1o_Passaggio       ?(già fatto)?
129000131206
129100131206         // -?Apertura archivio?
129200131206         If  NOT  %open(FNEVB04L);
129300131206           open(e)  FNEVB04L;
129400131206           if  %error;
129500131206             exsr  sr_ErrOpenFile;
129600131206           endif;
129700131206         EndIf;
129800131206
129900131206         // -?Ciclo di lettura per ricercare l'eventuale 1° passaggio?
130000131210         setgt   %kds(keyFNEVB04 : 4)  FNEVB000;
130100131210         readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
130200131206
130300131206         DoW  Not %eof(FNEVB04L)  and  Not $1o_Passaggio;
130400131206
130500131206           dEVB01 = EVBnot;
130600131206           $1o_Passaggio = (dEVB01.§NOTprg = 'P');
130700131206
130800131206           if  Not $1o_Passaggio;
130900131210             readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
131000131206           endif;
131100131206
131200131206         EndDo;
131300131206
131400131206       ENDSR;
131500131206
131600131206      /end-free
131700030515      *
131800030515      *---------------------------------------------------------------*
131900050217      *?Imposta i campi di stampa con quelli della DS                ?*
132000030515      *---------------------------------------------------------------*
132100110208     c     sr_PrepPRT    BEGSR
132200040223      *
132300040223      * Reperimento dati da FIAR5 / "GEN"
132400130924     c***?(già fatto:)?  exsr      RepAR5gen
132500070212     *** *
132600070212     *** * Impostazione Data spedizione
132700110208     *** * (già fatto nella subr. "sr_TipoLdV")
132800030515      *
132900030515      * - Intestazione moduli
133000030515      *
133100030515      * - Sigla operatore video
133200030515     c                   eval      Psop   =  DB0sop
133300081120      /free
133400081120         //
133500130411         // -?Flag "allega documento di trasporto"?
133600081120         select;
133700081120
133800130411           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
133900081120           when  DB0bam =  'S'   and   wB0tmc <> *blank;
134000081120             evalr Pddt =  'DDT sul collo';
134100081120
134200130411           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
134300081120           when  DB0bam =  'S';
134400081120             evalr Pddt =  'DDT';
134500081120
134600130716           // -?L'indicazione di packing-list è stata spostata in PPKL?
134700130716           //// -?Esiste packing-list abbinata?
134800130411           //when  $FIAR8 =  *on   and   $§PDF =  *off;
134900131203           //  evalr Pddt =  'PACKING-LIST';
135000130716           //// -?LdV con PDF allegato?
135100130411           //when  $FIAR8 =  *off  and   $§PDF =  *on;
135200130411           //  evalr Pddt =  'PACKING-LIST PDF';
135300130716           //// -?LdV con packing-list abbinata & PDF allegato?
135400130411           //when  $FIAR8 =  *on   and   $§PDF =  *on;
135500130411           //  evalr Pddt =  'PACKING-LIST PDF';
135600130411
135700130411           // -?LdV senza PDF allegato & senza packing-list abbinata?
135800081120           other;
135900081120             clear Pddt;
136000081120
136100081120         endsl;
136200081120
136300081120      /end-free
136400030515      *
136500030515      * - Mittente
136600170517     c                   if        *in71
136700170517     c                   exsr      sr_Mitt_DPD
136800170517     c                   endif
136900170517      *
137000030515     c                   if        DB0ccm <> *zeros
137100030515     c                   move      DB0ccm        Pccm
137200030515     c                   endif
137300030515     c                   eval      Prsm   =  DB0rsm
137400030515     c                   eval      Pinm   =  DB0inm
137500030515     c                   eval      Pcam   =  DB0cam
137600030515     c                   eval      Plom   =  DB0lom
137700030515     c                   eval      Pprm   =  DB0prm
137800030515     c                   eval      Pnzm   =  DB0nzm
137900030515      *   Mittente originale
138000030515if  1c                   if        DB0rmo <> *blanks
138100080430     c                   eval      Pkmo   =  'Mitt.Originale '
138200030515     c                   eval      Prmo   =  DB0rmo
138300030515e   1c                   endif
138400030515      *
138500070308      * - Filiale di Arrivo
138600030515     c                   move      DB0lna        Plna
138700030515      *
138800030515      * - Destinatario
138900030515if  1c                   if        DB0ccd <> *zeros
139000030515     c                   move      DB0ccd        Pccd
139100030515e   1c                   endif
139200030515     c                   eval      Prsd   =  DB0rsd
139300030515     c                   eval      Prse   =  DB0rd2
139400030515     c                   eval      Pind   =  DB0ind
139500030515     c                   eval      Pcad   =  DB0cad
139600030515     c                   eval      Plod   =  DB0lod
139700030515     c                   eval      Pprd   =  DB0prd
139800030515     c                   eval      Pnzd   =  DB0nzd
139900030515      *
140000030515      * - Codice tariffa
140100030515if  1c                   if        %subst(DB0tb1:1:1) = 'A'
140200030515      *   - nel box del destinatario se porto assegnato
140300030515     c                   move      DB0ctr        Pcta
140400030515x   1c                   else
140500030515      *   - nel box del mittente se porto franco
140600030515     c                   move      DB0ctr        Pctf
140700030515e   1c                   endif
140800030515      *
140900030515      * - Riferimenti mittente
141000080430     c                   evalr     Prmn   =  %char(DB0rmn)
141100030515     c                   eval      Prma   =  DB0rma
141200171117      *//* se bolla DPD recupero il riferimento da FIAR4 rcd "I"
141300171117      * se bolla DPD recupero il riferimento da FIPND00F
141400171117     c*//***?(già fatto:)?  clear                   dsBL4I
141500060626     c                   if        *in71
141600171117     c*//                exsr      RepAR4_I
141700171117     c                   exsr      RepFIPND
141800060626     c                   endif
141900030515      *
142000030515      * - Nominativo & telefono
142100031124      *   (da FIAR5 / "GEN")
142200130924     c***?(già fatto:)?  exsr      RepAR5gen
142300031124     c                   eval      Pnom   =  §AR5ref
142400031124     c                   eval      Ptel   =  §AR5teld
142500030515      *
142600030515      * - Spedizione
142700030515      *   (numero: non se Rimesso al Mittente)
142800030515if  1c                   if        DB0ris <> 'M'
142900080430     c                   eval      Plnp   =  %editc(DB0lnp : 'X')
143000080430     c                   eval      Pnrs   =  %editc(DB0nrs : 'Z')
143100080430     c                   eval      Pnsp   =  %editc(DB0nsp : 'Z')
143200030515e   1c                   endif
143300030515      * . . Data spedizione
143400031124     c     *iso          movel     Wdsp          Data_EUR
143500080506     c                   eval      PdspA = %editc( %dec(Data_EUR) : 'Y' )
143600030515      *
143700030515      * - Colli: numero, peso, volume, flag tipo-volume
143800080430     c                   eval      PnclA  =  %editc(DB0ncl : 'Z')
143900080911      /free
144000080911           if  NOT *in71;
144100080911      /end-free
144200080430     c                   eval      PpkfA  =  %editc(DB0pkf : '4')
144300080430     c                   eval      PvlfA  =  %editc(DB0vlf : '4')
144400030515if  1c                   if            DB0fvf <> *blanks
144500030515     c                             and DB0ris <> 'M'
144600081021     c                   movel     DB0fvf        Pfvf
144700030515e   1c                   endif
144800080915      /free
144900080915           endif;
145000080915      /end-free
145100030515      *
145200030515      * - Natura merce
145300030515     c                   eval      Pnat   =  DB0nas
145400030515      *
145500030515      * - Servizio
145600100517      /free
145700100517         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
145800110117           exsr  sr_CheckORM_PreavvMail;
145900100517         endif;
146000100517      /end-free
146100100517      * . . Descrizione porto
146200130925      *     (NON se presente ORM con preavviso mail)
146300100517     c                   if        Not  $PreavvORM
146400030515     c                   eval      Pds1   =  DB0dcb
146500100517     c                   endif
146600030515      * . . Descrizione tipo servizio
146700030515if  1c                   if        DB0fst =  'S'
146800030515     c                   eval      Pds2   =  DB0dts
146900030515e   1c                   endif
147000030515      * . . Reso / Franco frontiera
147100030515sel 1c                   select
147200030515w   1c                   when      DB0fbr =  'S'
147300030515     c                   clear                   w008a
147400030515     c                   eval      w008a  =  '   RESO '
147500030515     c                   move      w008a         Pds2
147600030515w   1c                   when      DB0fbr =  'F'
147700030515     c                   clear                   w015a
147800030515     c                   eval      w015a  =  '   FR.FRONTIERA'
147900030515     c                   move      w015a         Pds2
148000030515e   1c                   endsl
148100030515      *
148200080430      * - Zona consegna
148300080430      *   (campo alfanumerico)
148400080430     c                   eval      Pznc   =  %editc(DB0znc : 'X')
148500030515      *
148600030515      * - Flag "*" (gestito a parte)
148700040223      *
148800040223      * - Flag "V" = colli di valore
148900040223if  1c                   if        §AR5bva   <> *blank
149000040223     c                   eval      Pbva   =  'V'
149100040223e   1c                   endif
149200030515      *
149300030515      * - Fermo deposito
149400030515if  1c                   if        DB0ffd <> *blanks
149500080430     c                   eval      Pgc2   =  'FERMO DEPOSITO '
149600030515e   1c                   endif
149700030515      *
149800070308      * - Segnacolli: filiale, serie, dal / al
149900030515if  1c                   if        DB0fls <> *zeros
150000080430     c                   eval      Ppos   =  %editc(DB0fls : 'X')
150100030515e   1c                   endif
150200040607      * - - se bolla con mamma considero la serie della mamma
150300070515if  1c                   if        $Mamma =  *on
150400080430     c                   eval      Psrs   =  %editc(wB0nrs : 'Z')
150500031015x   1c                   else
150600080430     c                   eval      Psrs   =  %editc(DB0nrs : 'Z')
150700031015e   1c                   endif
150800030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
150900030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
151000030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
151100030515      *   Se collo singolo .......:   "     SCD
151200031015sel 1c                   select
151300030515      * - - non sequenziali
151400030515w   1c                   when      DB0fns =  'S'
151500131008if  2c                   if        DB0ncl <= 2
151600050511     c                   exsr      ListaScNSd
151700131008x   2c                   else
151800081021     c                   eval      Pnsx   =  'NON SEQUENZIALI'
151900131008e   2c                   endif
152000030515      * - - singolo o sequenziali
152100030515      *     (non se rimesso al mittente)
152200030515w   1c                   when      DB0ris <> 'M'
152300080430     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
152400030515if  2c                   if            DB0nca <> *zeros
152500030515     c                             and DB0nca <> DB0ncd
152600080430     c                   eval      Pnsx   =  %trimr(Pnsx) + ' - '
152700080430     c                                    +  %editc(DB0nca : 'Z')
152800030515e   2c                   endif
152900030515e   1c                   endsl
153000030515      *
153100131016      * - Note (1 e 2) e Particolarità (3, 4, 5 e 6)
153200030515     c                   exsr      SchPARTIC
153300131016e   1 *
153400030716if  1c                   if           $NP(1) <> *blanks
153500030716     c                             or $NP(2) <> *blanks
153600030716     c                             or $NP(3) <> *blanks
153700030716     c                             or $NP(4) <> *blanks
153800030716     c                             or $NP(5) <> *blanks
153900030716     c                             or $NP(6) <> *blanks
154000030710     c                   eval      Pnp1   =  $NP(1)
154100030710     c                   eval      Pnp2   =  $NP(2)
154200030710     c                   eval      Pnp3   =  $NP(3)
154300030710     c                   eval      Pnp4   =  $NP(4)
154400030710     c                   eval      Pnp5   =  $NP(5)
154500030710     c                   eval      Pnp6   =  $NP(6)
154600030716e   1c                   endif
154700030515      *
154800080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
154900080401      *   (servizio consegna garantita x cliente Wuerth)
155000170413if  1c*//                if        DB0gva =  'P '
155100170413     c*//                movel     'PRIVATO'     Ppad
155200170413e   1c*//                endif
155300041012if  1c                   if        DB0gva =  'U '
155400041013     c                   eval      Ppad = 'CONSEGNA AD UFFICI'
155500041012e   1c                   endif
155600030515if  1c                   if           DB0tc1 =  'A'
155700030515     c                             or DB0tc2 =  'A'
155800030515     c                   move      'APPUNTAMENTO'Ppad
155900030515e   1c                   endif
156000080401if  1c                   if             DB0nrs = 40
156100090518     c                             and  DB0tsp = 'E'
156200080401     c                             and (DB0lnp = 043
156300080401     c                              or  DB0lnp = 006
156400080401     c                              or  DB0lnp = 063)
156500090304     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
156600090304     c                                     WÜRTH'
156700080401e   1c                   endif
156800030515      *
156900030515      * - Data e ora consegna richiesta
157000030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
157100091116      *   (NO "Prima del" per il cliente TTL)
157200091116if  0c                   if        DB0ccm <> c_CliTTL  or
157300091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
157400030515if  1c                   if        DB0dcr <> *zeros
157500030515     c                   eval      Past   =  '*'
157600030515     c     *iso          movel     DB0dcr        Data_EUR
157700030515     c     *eur          movel     Data_EUR      Pdcr
157800030515e   1c                   endif
157900030515if  1c                   if        DB0hcr <> *zeros
158000030515     c                   eval      Past   =  '*'
158100080430     c                   eval      Phcr   =  %editw(DB0hcr : '0 :  ')
158200030515e   1c                   endif
158300091116e   0c                   endif
158400030515      *
158500030515      * - Consegne particolari
158600030515     c                   eval      Pcp1   =  DB0dc1
158700030515     c                   eval      Pcp2   =  DB0dc2
158800030515      *
158900030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
159000030515      *   (importi "arrotondati" a 2 decimali in stampa)
159100030515if  1c                   if        DB0ift <> *zeros
159200080515     c                   eval      Pkim   = 'Imponibile'
159300080430     c                   eval(h)   w0112  =  DB0imv
159400080430     c                   eval      Pimp   =  %editc(w0112 : 'K')
159500030515sel 2c                   select
159600030515w   2c                   when      DB0iva <> *zeros
159700080515     c                   eval      Pkie   = 'I.v.a.    '
159800030624     c                   eval(h)   w0112  =  DB0iva
159900080430     c                   eval      Piva   =  %editc(w0112 : 'K')
160000030515w   2c                   when         DB0cei <> *blanks
160100030515     c                             or DB0dce <> *blanks
160200130213     c                   eval      Pkie   = 'Esenzione '
160300130213     c                   eval      Pese   = '           ' +
160400130213     c                                      %subst(DB0dce : 1 : %len(§EIdes))
160500030515e   2c                   endsl
160600080515     c                   eval      Pktf   = 'Tot. Fatt.'
160700080430     c                   eval(h)   w0112  =  DB0ift
160800080430     c                   eval      Pift   =  %editc(w0112 : 'K')
160900030515if  2c                   if        DB0div <> §GEDcn
161000030515     c                   eval      Pdiv   =  DB0div
161100030515e   2c                   endif
161200030515e   1c                   endif
161300030515      *
161400030515      * - Contrassegno e relativa divisa
161500030515if  1c                   IF        DB0cas <> *zeros
161600030625      *
161700080430     c                   eval(h)   w0112  =  DB0cas
161800080430     c                   eval      Pcas   =  %editc(w0112 : 'K')
161900030515if  2c                   if        DB0vca <> §GEDcn
162000030515     c                   eval      Pdca   =  DB0vca
162100030515e   2c                   endif
162200030515     c                   exsr      CtrCAs
162300030624      * - - Nuovo controllo abilitazione c/assegno
162400030624     c                   reset                   TRUL21DS
162500030624     c                   eval      I21cbo =  DB0cbo
162600030624     c                   eval      I21tsp =  DB0tsp
162700030624     c                   eval      I21lnp =  DB0lnp
162800030624     c                   eval      I21nzm =  DB0nzm
162900030624     c                   eval      I21lna =  DB0lna
163000030624     c                   eval      I21nzd =  DB0nzd
163100130826     c                   if        DB0ccm >  *zero
163200130826     c                   eval      I21ksc =  DB0ccm
163300130826     c                   else
163400130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
163500130826     c                   endif
163600030624     c                   eval      I21tic =  DB0tic
163700030624if  2c                   if        DB0vca <> §GEDcn
163800030624     c                   eval      I21imp =  Wcas
163900030624     c                   eval      I21div =  §GEDcn
164000030624x   2c                   else
164100030624     c                   eval      I21imp =  DB0cas
164200030624     c                   eval      I21div =  DB0vca
164300030624e   2c                   endif
164400080430     c                   eval      I21ute =  SDSusr
164500040607     c                   eval      I21pgm =  SDSpgm
164600030624     c                   call      'TRUL21R'
164700030624     c                   parm                    TRUL21ds
164800030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
164900030624     c                   exsr      Tab_1A
165000030624     c                   movel     §1Ades        DB0dti
165100030624if  2c                   if            §1Adeo <> *blanks
165200030624     c                             and O21fca <> *blanks
165300030624     c                   movel     §1Adeo        DB0dti
165400030624e   2c                   endif
165500030624      * - - Imposto i campi di stampa
165600080430     c                   eval      Pcti   =  DB0tic
165700080430     c                   eval      Ptic   =  DB0dti
165800030625      *
165900030515e   1c                   ENDIF
166000030515      *
166100030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
166200030515if  1c                   IF        DB0ift >  *zeros
166300030625      *
166400030515if  2c                   if            DB0vca <> §GEDcn
166500030515     c                             and DB0vca <> *blanks
166600030515     c                   eval      Pdie   =  DB0div
166700030515e   2c                   endif
166800030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
166900030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
167000030515      *   (importi "arrotondati" a 2 decimali in stampa)
167100030515sel 2c                   Select
167200110204      *   Non ho C/Assegno
167300101213w   2c                   When      DB0cas  = *zeros
167400030624     c                   eval(h)   w0112  =  DB0ift
167500030624     c                   eval      Pies   =  %editc(w0112 : 'K')
167600030625     c                   clear                   Pie1
167700030625     c                   clear                   Pdi1
167800030515      *   C/Assegno sommabile
167900030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
168000030515sel 3c                   select
168100030515      *   - c/assegno in EURO
168200030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
168300030624     c                   eval(h)   w0112  =  DB0ift + DB0cas
168400030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168500030624     c                   clear                   Pie1
168600030624     c                   clear                   Pdi1
168700030515      *   - c/assegno in DIVISA
168800030515w   3c                   when          DB0div =  §GEDcn
168900030515     c                             and DB0vca <> §GEDcn
169000030624     c                   eval(h)   w0112  =  DB0ift + Wcas
169100030624     c                   eval      Pies   =  %editc(w0112 : 'K')
169200030624     c                   clear                   Pie1
169300030624     c                   clear                   Pdi1
169400030515e   3c                   endsl
169500100218      *   C/Assegno non sommabile
169600030515x   2c                   Other
169700030624     c                   eval(h)   w0112  =  DB0cas
169800030624     c                   eval      Pie1   =  %editc(w0112 : 'K')
169900100218     c*//                eval      Pdi1   =  DB0vca
170000030624     c                   eval(h)   w0112  =  DB0ift
170100030624     c                   eval      Pies   =  %editc(w0112 : 'K')
170200100218     c*//                eval      Pdie   =  DB0div
170300030515e   2c                   Endsl
170400030709      *
170500030709e   1c                   ENDIF
170600030515      *
170700030515      * - Numero e data fattura
170800030515if  1c                   if        DB0nft <> *zeros
170900080430     c                   eval      Pkft   =  'Fatt.'
171000080430     c                   eval      Pnft   =  %editc(DB0nft : 'Z')
171100130205     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
171200130205     c                                      ')'
171300030515if  2c                   if        DB0dft >  *zeros
171400080430     c                   eval      Pkdf   =  ' del '
171500030515     c     *iso          movel     DB0dft        Data_EUR
171600130213     c     *dmy          movel     Data_EUR      Pdft
171700030515e   2c                   endif
171800030515e   1c                   endif
171900030515      *
172000030515      * Memorizzo il codice tariffa come numero di copie da stampare
172100030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
172200030515if  1c                   if        DB0ris = 'M'  and  *in70
172300030515     c                   eval      Wctr   =  DB0ctr
172400030515e   1c                   endif
172500030515      *
172600030515     c                   ENDSR
172700170517      *
172800170517      /free
172900170517       //--------------------------------------------------------------
173000170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
173100170517       //?spedizione DPD.                                              ?
173200170517       //--------------------------------------------------------------
173300170517       BEGSR  sr_Mitt_DPD;
173400170517
173500170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
173600170517         clear  dCLI;
173700170517         reset  TIBS02ds;
173800170517         T02cod = 'CLI';
173900170517         T02ke1 = %editc( DB0ccm : 'X' );
174000170517         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
174100170517         if  T02err = *blank;
174200170517           dCLI = T02uni;
174300170517         endif;
174400170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
174500170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
174600170517           leavesr;
174700170517         endif;
174800170517
174900170517         // -?Forzatura Mittente da note "AM"?
175000170517         If  §CLItfntc = 'S';
175100170517
175200170517           if  NOT %open( TFNTC01L );
175300170517             open  TFNTC01L;
175400170517           endif;
175500170517           clear  keyTFNTC01;
175600170517           k_NTCapl = 'C';
175700170517           k_NTCnk1 = %editc( c_Kci : 'X' ) + %editc( DB0ccm : 'X' );
175800170517           k_NTCtnt = 'AM';
175900170517           chain  %kds( keyTFNTC01 )  TFNTC;
176000170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
176100170517             DB0rsm = NTCrnt;
176200170517           endif;
176300170517
176400170517         EndIf;
176500170517
176600170517         // -?Pulizia Indirizzo Mittente?
176700170517         If  §CLInoIMtt = 'S';
176800170517
176900170517           clear  DB0inm;
177000170517           clear  DB0cam;
177100170517           clear  DB0lom;
177200170517           clear  DB0prm;
177300170517           clear  DB0nzm;
177400170517
177500170517         EndIf;
177600170517
177700170517       ENDSR;
177800170517
177900170517      /end-free
178000030515      *
178100030515      *---------------------------------------------------------------*
178200050217      *?Inschieramento delle Note e delle Particolarità              ?*
178300030515      *---------------------------------------------------------------*
178400030515     c     SchPARTIC     BEGSR
178500030515      *
178600130924     c***?(già fatto:)?  reset                   $NoDesPartic
178700130924     c***?(già fatto:)?  clear                   $NP
178800130924     c***?(già fatto:)?  clear                   W_CoOr
178900030515     c                   clear                   XX
179000030710      *
179100030710      * Imposto eventuali note per prime
179200030710if  1c                   if        DB0nt1 <> *blanks
179300030710     c                   add       1             xx
179400030710     c                   movel     DB0nt1        $NP(xx)
179500030710e   1c                   endif
179600030710if  1c                   if        DB0nt2 <> *blanks
179700030710     c                   add       1             xx
179800030710     c                   movel     DB0nt2        $NP(xx)
179900030710e   1c                   endif
180000030515      *
180100030515      * Le particolarità possono essere lunghe fino a 45 char.
180200030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
180300030515      *   bastare, il resto lo si mette nella riga successiva.
180400030710      *   Se, invece, non risultano decodificate, si mettono solo i
180500030710      *   codici sulla stessa riga.
180600030710      *   Se uno solo risulta decodificato, esso va su una riga a
180700030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
180800030710      *                                    *on  = ok riga procedente)
180900030515      *
181000030515      * . . Particolarità Giacenza
181100030515if  1c                   IF        DB0gga <> *blanks
181200030515     c                   movel     'Q'           WTpPar
181300030515     c                   movel     DB0gga        WCdPar
181400030515     c                   exsr      TABxPAR
181500030710if  2c                   if            TBLuni       = *blanks
181600030710     c                             and $NoDesPartic = *on
181700030710     c                   eval      $NP(xx) = %trim($NP(xx))
181800030710     c                                     + '   ' + WCdPar
181900030710x   2c                   else
182000030710if  3c                   if        TBLuni       = *blanks
182100030710     c                   eval      $NoDesPartic = *on
182200030710x   3c                   else
182300030710     c                   eval      $NoDesPartic = *off
182400030710e   3c                   endif
182500030515     c                   add       1             xx
182600100517     c                   movel     w053a         $NP(xx)
182700100517if  3c                   if        %subst(w053a : %len($NP(xx))+1)
182800030515     c                              <> *blanks
182900030515     c                   add       1             xx
183000030515     c                   eval      $NP(xx) = '   ' +
183100100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
183200030710e   3c                   endif
183300030515e   2c                   endif
183400030515e   1c                   ENDIF
183500030515      *
183600030515      * . . Particolarità Consegna
183700030515if  1c                   IF        DB0gma <> *blanks
183800030515     c                   movel     'R'           WTpPar
183900030515     c                   movel     DB0gma        WCdPar
184000030515     c                   exsr      TABxPAR
184100030710if  2c                   if            TBLuni       = *blanks
184200030710     c                             and $NoDesPartic = *on
184300030710     c                   eval      $NP(xx) = %trim($NP(xx))
184400030710     c                                     + '   ' + WCdPar
184500030710x   2c                   else
184600030710if  3c                   if        TBLuni       = *blanks
184700030710     c                   eval      $NoDesPartic = *on
184800030710x   3c                   else
184900030710     c                   eval      $NoDesPartic = *off
185000030710e   3c                   endif
185100030515     c                   add       1             xx
185200100517     c                   movel     w053a         $NP(xx)
185300100517if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
185400030515     c                              <> *blanks
185500030515     c                   add       1             xx
185600030515     c                   eval      $NP(xx) = '   ' +
185700100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
185800030710e   3c                   endif
185900030515e   2c                   endif
186000030515e   1c                   ENDIF
186100030515      *
186200030515      * . . Particolarità Varie
186300030717      *     - questa descrizione va messa nell'apposito campo PPAD
186400041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
186500030717      *     - se 'B' (bancali da rendere al mittente) comparirà un
186600030717      *       apposito box
186700030717      *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
186800030717      *       comparirà un apposito box, un'apposita nota e l'indica-
186900030717      *       zione nella copia per il destinatario.
187000030717      *     ...Potrebbero anche non esserci più elementi di schiera
187100030717      *     disponibili [xx >= %elem($NP)] !
187200030709if  1c                   IF            DB0gva <> *blanks
187300030717     c                             and DB0gva <> 'B '
187400030717     c                             and DB0gva <> 'O '
187500030709     c                             and DB0gva <> 'P '
187600041015     c                             and DB0gva <> 'U '
187700030515     c                   movel     'S'           WTpPar
187800030515     c                   movel     DB0gva        WCdPar
187900030515     c                   exsr      TABxPAR
188000030710if  2c                   if            TBLuni       = *blanks
188100030710     c                             and $NoDesPartic = *on
188200030710     c                   eval      $NP(xx) = %trim($NP(xx))
188300030710     c                                     + '   ' + WCdPar
188400030710x   2c                   else
188500030710if  3c                   if        TBLuni       = *blanks
188600030710     c                   eval      $NoDesPartic = *on
188700030710x   3c                   else
188800030710     c                   eval      $NoDesPartic = *off
188900030710e   3c                   endif
189000030717if  3c                   if            xx < %elem($NP)
189100030709     c                   add       1             xx
189200100517     c                   movel     w053a         $NP(xx)
189300100517if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
189400030709     c                              <> *blanks
189500030717     c                             and xx < %elem($NP)
189600030709     c                   add       1             xx
189700030709     c                   eval      $NP(xx) = '   ' +
189800100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
189900030717e   4c                   endif
190000030717e   3c                   endif
190100030709e   2c                   endif
190200030515e   1c                   ENDIF
190300030717      *
190400030717      * Imposto il numero originale di colli componenti la spedizione
190500030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
190600030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
190700030717     c                   exsr      RepAR5bnb
190800030717if  2c                   if        W_CoOr  <> *blanks
190900030717     c                             and xx  <  %elem($NP)
191000030717     c                   add       1             xx
191100030717     c                   eval      $NP(xx) =  W_CoOr
191200030717e   2c                   endif
191300030717e   1c                   endif
191400030515      *
191500030515     c                   ENDSR
191600070319      *
191700070319      *---------------------------------------------------------------*
191800070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
191900070319      *---------------------------------------------------------------*
192000070319     c     SchConsParz   BEGSR
192100070319      *
192200130924     c***?(già fatto:)?  reset                   $CP
192300130924     c***?(già fatto:)?  reset                   $CPdcm
192400070319     c                   clear                   XX
192500070319      *
192600070319if  2c                   if        NOT %open(FNART01L)
192700101213     c                   open(e)   FNART01L
192800101213      /free
192900101213             if  %error;
193000101213               exsr  sr_ErrOpenFile;
193100101213             endif;
193200101213      /end-free
193300070319e   2c                   endif
193400070319      *
193500070514if  2c                   if        $Mamma = *off
193600070319     c     KLDV          setll     FNART000
193700070319     c     KLDV          reade     FNART000
193800070514x   2c                   else
193900070514     c     KLBL          setll     FNART000
194000070514     c     KLBL          reade     FNART000
194100070514e   2c                   endif
194200070319      *
194300070319do  1c                   DOW       NOT  %eof(FNART01L)
194400070319      *
194500070319if  2c                   If             ARTatb = *blanks
194600070319     c                             and  ARTdcm > *zeros
194700070319      *
194800070319     c                   eval      *in31  = *off
194900070319     c                   eval      xx     = 1
195000070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
195100070319if  3c                   if        NOT  *in31
195200070319     c                   eval      xx     = 1
195300070319     c     *zeros        lookup    $CPdcm(xx)                             31
195400070319if  4c                   if        *in31
195500140304     c*//                eval      $CP(xx)     = *zeros
195600140304     c                   clear                   Elem_CP
195700140304     c                   movel     Elem_CP       $CP(xx)
195800070319e   4c                   endif
195900070319e   3c                   endif
196000070319if  3c                   if        *in31
196100070319     c                   eval      $CPdcm(xx)  = ARTdcm
196200070319     c                   movel     $CP(xx)       Elem_CP
196300070319     c                   eval      WCPdcm      = ARTdcm
196400070319     c                   eval      WCPncl      = WCPncl + 1
196500070319     c                   movel     Elem_CP       $CP(xx)
196600070319e   3c                   endif
196700070319      *
196800070319e   2c                   EndIf
196900070319      *
197000070514if  2c                   if        $Mamma = *off
197100070319     c     KLDV          reade     FNART000
197200070514x   2c                   else
197300070514     c     KLBL          reade     FNART000
197400070514e   2c                   endif
197500070319      *
197600070319e   1c                   ENDDO
197700070319      *
197800140514if  0c                   If        xx     > *zero
197900140513      * Reperimento nominativo firmatario   &
198000140514     c                   exsr      sr_Firmatario
198100070319      * Ordinamento schiera per data
198200070319if  1c                   if        xx     > 1
198300070319     c                   sorta                   $CP
198400070319e   1c                   endif
198500140514e   0c                   EndIf
198600070319      *
198700070319     c                   ENDSR
198800140303      *
198900140303      /free
199000140303
199100140303       //--------------------------------------------------------------
199200140303       //?Reperimento nome firmatario                                  ?
199300140303       //--------------------------------------------------------------
199400140303       BEGSR  sr_Firmatario;
199500140303
199600140303if  1    if  NOT  %open(FNEVB22L);
199700140303           open  FNEVB22L;
199800140303e   1    endif;
199900140303
200000140303         clear  keyFNEVB22;
200100140303         k_EVB2aas = DB0aas;
200200140303         k_EVB2lnp = DB0lnp;
200300140303         k_EVB2nrs = DB0nrs;
200400140303         k_EVB2nsp = DB0nsp;
200500140303         k_EVB2cev = 'P  ';
200600140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
200700140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
200800140303         DoW  Not  %eof(FNEVB22L);
200900140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
201000140304             yy = %lookup( EVB2dev : $CPdcm );
201100140304             if  yy > *zero;
201200140304               Elem_CP = $CP(yy);
201300140303               if  WCPocm  <= EVB2hev;
201400140303                 WCPocm  = EVB2hev;
201500140303                 WCPfir  = EVB2dati;
201600140304                 $CP(yy) = Elem_CP;
201700140303               endif;
201800140303             endif;
201900140303           endif;
202000140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
202100140303         EndDo;
202200140303
202300140303       ENDSR;
202400140303
202500140303      /end-free
202600031124      *
202700031124      *---------------------------------------------------------------*
202800050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
202900031124      *---------------------------------------------------------------*
203000031124     c     RepAR5gen     BEGSR
203100031124      *
203200031124     c                   clear                   dAR5gen
203300031124      *
203400031124     c                   eval      AR5trd = 'GEN'
203500031127     c     K05AR501      chain     FIAR5000
203600031124      *
203700101213if  1c                   if            %found(FIAR501L)
203800040301     c                             and AR5atb =  *blanks
203900031124     c                   movel     AR5uni        dAR5gen
204000031124e   1c                   endif
204100031124      *
204200031124     c                   ENDSR
204300030717      *
204400030717      *---------------------------------------------------------------*
204500050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
204600030717      *---------------------------------------------------------------*
204700030717     c     RepAR5bnb     BEGSR
204800030717      *
204900030717     c                   clear                   dAR5bnb
205000130924     c***?(già fatto:)?  clear                   W_CoOr
205100030717      *
205200030717     c                   eval      AR5trd = 'BNB'
205300030717     c     K05AR501      chain     FIAR5000
205400030717      *
205500101213if  1c                   if            %found(FIAR501L)
205600040301     c                             and AR5atb =  *blanks
205700030717     c                   movel     AR5uni        dAR5bnb
205800030717if  2c                   if        §AR5bcor <> *zeros
205900030717     c                   eval      W_CoOr   =  C_CoOr
206000030717     c                                      +  %editc(§AR5bcor : 'K')
206100030717e   2c                   endif
206200030717e   1c                   endif
206300030717      *
206400030717     c                   ENDSR
206500030515      *
206600030515      *---------------------------------------------------------------*
206700051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
206800030515      *---------------------------------------------------------------*
206900110208     c     sr_DecodLDV   BEGSR
207000030515      *
207100030515      * Richiesta firma per ricevuta della merce senza riserve
207200030515      * (e NON  quella  per quietanza) => *in35 + not *in36
207300030515     c                   movea     '10'          *in(35)
207400070307      *
207500070307      * Intestazione moduli
207600090619      *
207700090619      /free
207800121018
207900121018         // -?Indicazione della Partiva IVA o del Cod. Fiscale del Destinatario?
208000121018         //  ?se Porto Assegnato con Fattura immediata?
208100121018         //  ?- NON se destinatario estero -?
208200121018             // -?con fattura?
208300121018         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
208400121018             // - -?P.A. (con fattura immediata)?
208500121018             //    ?(con destinatario italiano)?
208600121018             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
208700121018             // - -?con 2ª bolla?
208800121018             //    ?(con destinatario italiano)?
208900131203              (DB0tb2 <> *blank         and  DB0nzd  = *blank) );
209000121018           select;
209100131010             // ·?Esiste Partita Iva NON FITTIZIA?
209200131010             when  wB0piv <> *blank  and
209300121026                   %scan('$$' : wB0piv) = *zero;
209400121018               PpivD = 'Partita Iva: ' + wB0piv;
209500131010             // ·?Esiste Codice Fiscale?
209600131010             when  wB0cfi <> *blank;
209700121018               PpivD = 'Codice Fiscale: ' + wB0cfi;
209800121018           endsl;
209900121018         ENDIF;
210000110204
210100110204         // -?Consegna al mattino (tempi consegna = 12 ore)?
210200110204         //  ?(NON per Würth)?
210300110204         If  §VPOrcm <= wDate and
210400090619             DB0tsp   = 'E'   and
210500090619            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
210600090619                                   DB0lnp <> 006   and
210700090619                                   DB0lnp <> 063));
210800090619           clear  TNSD99ds;
210900101213           D98tbo = 'A';
211000090619           D98aas = DB0aas;
211100090619           D98lnp = DB0lnp;
211200090619           D98nrs = DB0nrs;
211300090619           D98nsp = DB0nsp;
211400090619           tnsd99r ( TNSD99ds );
211500120910           if  D98tcc = 12;
211600090619             %subst(Pds2 : %len(Pds2) - 1) = ' M';
211700090619           endif;
211800110204         EndIf;
211900110204
212000110204         // -?"NON UTILIZZARE PER LA CONSEGNA"?
212100110204         //  ?(stampa della sola copia per il vettore)?
212200110204         if  $No_x_Cons = *on;
212300110204           *in35 = *off;
212400110204           //*in36 = *off;
212500110204           // -?costante da stampare se LDV recapito c/assegni stampata?
212600110204           //  ?dal pgm "stampa DDT NO in arrivo"?
212700110204           //Pnux   =  ' NON UTILIZZARE PER LA CONSEGNA';
212800110204         endif;
212900110204
213000110204         // -?Decodifica "Riserva Consegna" (se c'è)?
213100110204         clear  ds2A;
213200110204         If  ILVriserv <> *blank;
213300110204           TBLcod = '2A';
213400110204           TBLkey = ILVriserv;
213500110207           chain  K03TAB  TABEL;
213600110204           if  %found(TABEL00F);
213700110204             ds2A = TBLuni;
213800110204           endif;
213900110204         EndIf;
214000140514
214100140514         // -?Reperimento consegne parziali precedenti da FNART?
214200140514         If  $ConsParz;
214300140514           exsr  SchConsParz;
214400140514           if  xx < 1;
214500140514             $ConsParz = *off;
214600140514           endif;
214700140514         EndIF;
214800110204
214900110204         // -?Costante "PER RICEVUTA DELLA MERCE..."?
215000130716         exsr  sr_PerRicevutaDellaMerce;
215100110204
215200090619      /end-free
215300030515      *
215400130411      * Verifico se Packing-List e/o PDF
215500130411     c                   select
215600130411     c                   when      $§PDF  = *on
215700130411     c                   eval      Ppkl   =  'PACKING LIST PDF'
215800130411     c                   when      $FIAR8 =  *on
215900130411     c                   eval      Ppkl   =  'PACKING LIST    '
216000130411     c                   endsl
216100030515      *
216200030515      * Flag "*" > x Consegne particolari
216300030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
216400030515     c                   eval      Past   =  '*'
216500030515     c                   endif
216600030515      *
216700030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
216800030515if  1c                   IF        DB0cas <> *zeros
216900030515      * - Flag "*" > x C/Assegno
217000030904if  2c                   if        O21fca <> *blanks
217100030904     c                   movel     '*'           Past
217200030515e   2c                   endif
217300030624      * - Imposto i campi di stampa
217400080430     c                   eval      Pcti   =  DB0tic
217500080430     c                   eval      Ptic   =  DB0dti
217600030515e   1c                   ENDIF
217700030515      *
217800040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
217900030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
218000030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
218100030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
218200030515     c                             or        DB0tb2     <> *blanks
218300030515     c                   eval      Pass   =  'A'
218400030515if  2c                   if        DB0nft =  *zeros
218500080430     c                   if        Pkie   =  *blanks
218600080430     c                   evalr     Pkie   =  'SEGUE'
218700080430     c                   eval      Piva   =  ' FATTURA'
218800080430     c                   else
218900080430     c                   eval      Piva   =  'SEGUE FATTURA'
219000080430     c                   endif
219100030515e   2c                   endif
219200030515e   1c                   endif
219300030515      *
219400030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
219500051129if  1c                   IF            DB0dcr >  *zeros
219600051129     c                             or  DB0hcr >  *zeros
219700051129     c                   clear                   FNLV80ds
219800051129     c                   eval      ILV80aas   =  DB0aas
219900051129     c                   eval      ILV80lnp   =  DB0lnp
220000051129     c                   eval      ILV80nrs   =  DB0nrs
220100051129     c                   eval      ILV80nsp   =  DB0nsp
220200051129     c                   eval      ILV80mgs   =  DB0mgs
220300051129     c                   eval      ILV80tc1   =  DB0tc1
220400051129     c                   eval      ILV80tc2   =  DB0tc2
220500051129     c                   eval      ILV80dcr   =  DB0dcr
220600051129     c                   eval      ILV80hcr   =  DB0hcr
220700051129     c                   eval      ILV80tcr   =  DB0tcr
220800051129     c                   call      'FNLV80R'
220900051129     c                   parm                    KPJBA
221000051129     c                   parm                    FNLV80ds
221100051129sel 2c                   Select
221200051129      * errore
221300051129w   2c                   When      OLV80err  <>  *blanks
221400051129      * consegna tassativa (per ora solo ai supermercati)
221500051129w   2c                   When      OLV80tat   =  'S'
221600081021     c                   movel     OLV80tcrd     Ptdr
221700091116      * NO "Prima del" per il cliente TTL
221800091116w   2c                   When      DB0ccm     = c_CliTTL   and
221900091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
222000051129      * prima/dopo il/delle
222100051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
222200110208      *  nella subr. "sr_TipoLDV")
222300081022x   2c                   Other
222400081021     c                   movel     OLV80tcrd     Ptdr
222500081021e   2c                   Endsl
222600051129e   1c                   ENDIF
222700030515      *
222800030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
222900060921      * (SE?non?è "Fermo Deposito")
223000030515      * 1° turno di chiusura
223100060921if  1c                   IF             DB0gc1 <> *blanks
223200060921     c                             and  DB0ffd  = *blanks
223300030515      * - Flag "*" > Turno di chiusura
223400030515     c                   eval      Past   =  '*'
223500030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
223600030515      * - Nessun giorno specificato - Solo 'M' o 'P'
223700130924     c***?già così:?     clear                   Pgc1
223800030515if  3c                   if        DB0gc1 =  ' M'
223900030515     c                   eval      Pgc1   =  %trim($Gio(8))
224000030515x   3c                   else
224100030515     c                   eval      Pgc1   =  %trim($Gio(9))
224200030515e   3c                   endif
224300030515x   2c                   Else
224400030515      * - Giorno specificato
224500030515     c                   movel     DB0gc1        w001a
224600030515     c                   eval      XX     =  1
224700030515     c                   eval      *in31  =  *off
224800030515     c     w001a         lookup    $GgS(xx)                               31
224900030515     c   31              movel     $Gio(xx)      Pgc1
225000030515      *   + 'M' o 'P'
225100030515sel 3c                   select
225200030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
225300030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
225400030515     c                                    +  %trim($Gio(8))
225500030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
225600030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
225700030515     c                                    +  %trim($Gio(9))
225800030515e   3c                   endsl
225900030515e   2c                   Endif
226000030515e   1c                   ENDIF
226100030515      * 2° turno di chiusura
226200060921if  1c                   IF             DB0gc2 <> *blanks
226300060921     c                             and  DB0ffd  = *blanks
226400030515      * - Flag "*" > Turno di chiusura
226500030515     c                   eval      Past   =  '*'
226600030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
226700030515      * - Nessun giorno specificato - Solo 'M' o 'P'
226800130924     c***?già così:?     clear                   Pgc2
226900030515if  3c                   if        DB0gc2 =  ' M'
227000080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
227100080515     c                                                = %trim($Gio(8))
227200030515x   3c                   else
227300080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
227400080515     c                                                = %trim($Gio(9))
227500030515e   3c                   endif
227600030515x   2c                   Else
227700030515      * - Giorno specificato
227800030515     c                   movel     DB0gc2        w001a
227900030515     c                   eval      XX     =  1
228000030515     c                   eval      *in31  =  *off
228100030515     c     w001a         lookup    $GgS(xx)                               31
228200080515     c   31              eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
228300080515     c                                                = %trim($Gio(xx))
228400030515      *   + 'M' o 'P'
228500030515sel 3c                   select
228600030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
228700080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
228800030515     c                                    +  %trim($Gio(8))
228900030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
229000080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
229100030515     c                                    +  %trim($Gio(9))
229200030515e   3c                   endsl
229300030515e   2c                   Endif
229400030515e   1c                   ENDIF
229500080523      /free
229600080523       // ________________________________________
229700080523       // Impostazione nominativo del destinatario
229800080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
229900110214         exsr  RepAR4_1;
230000080523       // _______________________________________
230100080523       // Impostazione data ed ora della consegna
230200080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
230300110228         //?ATTENZIONE:?il "RESET" di un campo di tipo "d" (data)?
230400110228         //?riporta in tale campo la *date, NON quella impostata?
230500110228         //?come "inz(...)" della specifica "d" !!!!!!!!!!!!!!!!?
230600110228         Data_EUR = *loval;
230700110209         clear  w0080;
230800110209         select;
230900110209           when  wB0dcm > *zero;
231000110209             Data_EUR = %date( wB0dcm : *iso );
231100110209           when  wB0dcp > *zero;
231200110209             Data_EUR = %date( wB0dcp : *iso );
231300110209         endsl;
231400110209         if  Data_EUR > *loval;
231500110209           w0080 = ( ( %subdt(Data_EUR : *days) * 1000000 ) +
231600110209                     ( %subdt(Data_EUR : *months) * 10000 ) +
231700110209                     ( %subdt(Data_EUR : *years) ) );
231800110209         endif;
231900150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
232000081103           Pfedkcd  = %editc( w0080 : 'Y' );
232100120702           if  dILVlibero.§ILVhmp > *zero;
232200081103             Pfedkcd = %trimr( Pfedkcd ) + '  '
232300120702                     + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
232400120702                     + %subst( dILVlibero.§ILVhmp : 3 : 2 );
232500081103           endif;
232600081103         else;
232700081103           Pkcd  = %editc( w0080 : 'Y' );
232800120702           if  dILVlibero.§ILVhmp > *zero;
232900081103             Pkcd = %trimr( Pkcd ) + '   '
233000120702                  + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
233100120702                  + %subst( dILVlibero.§ILVhmp : 3 : 2 );
233200081103           endif;
233300081103         endif;
233400080523      /end-free
233500071114      *
233600071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
233700030515      *
233800071114      * Se pgm richiamato x stampa LdV in arrivo:
233900030515if  1c                   IF        DB0fl1 =  'A'
234000071114      * - imposto l'"*" in base al parametro ricevuto.
234100030515if  2c                   if        DB0fl2 =  '*'
234200030515     c                   eval      Past   =  '*'
234300030515x   2c                   else
234400030515     c                   clear                   Past
234500030515e   2c                   endif
234600071114      * - idem per flag di bolla non abilitata
234700030515     c                   eval      Pdis   =  DB0fl5
234800030515e   1c                   endif
234900030515      *
235000030515     c                   ENDSR
235100080430      *
235200080430      /free
235300130716
235400130716       //--------------------------------------------------------------
235500130716       //?Impostazione "PER RICEVUTA DELLA MERCE ..."                  ?
235600130716       //--------------------------------------------------------------
235700130716       BEGSR  sr_PerRicevutaDellaMerce;
235800130716
235900130716         Select;
236000130716
236100130716           // -?danon?stampare se?
236200130716           // ·?non utilizzare per la consegna  (NOT *in35)?
236300130716           When  NOT *in35;
236400130716             //clear  Pkrm;       ?(già fatto!)?
236500140108             //clear  Pkrm2;      ?(già fatto!)?
236600130716
236700130716           // -?costante per "DPD"   (come standard)?
236800130716           //         ?o per "FedEx" (come standard)?
236900130716           When  *in71  or  *in72;
237000130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
237100130716             // -?senza riserve?
237200130716             if  ILVriserv = *blank;
237300130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
237400130716             // -?con Riserva Consegna?
237500130716             else;
237600130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
237700130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
237800130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
237900130716               endif;
238000130716             endif;
238100130716
238200130716           // -?costante per cliente?
238300130716           //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
238400140513           When  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
238500130716             exsr  sr_RepDdtCLN;
238600130716             Pkrm = 'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
238700130716                     DDT '
238800130716                  + %trimr( %editc (WddtCLN : 'Z') )
238900130716                  + ' DEL '
239000130716                  + %editc (WdspCLN : 'Y')
239100130716                  + ' CON RISERVA DI CONTROLLO COME CONTRATTO';
239200130716             if  Wdsp < 20101001;
239300130716               Pkrm = %trimr(Pkrm) + ' C.L.N.';
239400130716             else;
239500130716               Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
239600130716             endif;
239700130716
239800130716           // -?costante "standard" se "DDT sì"?
239900130716           When  §VPOrsr <= wDate  and  DB0bam = 'S';
240000130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
240100130716                     TRASPORTO';
240200130716             // -?senza riserve?
240300130716             if  ILVriserv = *blank;
240400130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
240500130716             // -?con Riserva Consegna?
240600130716             else;
240700130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
240800130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
240900130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
241000130716               endif;
241100130716             endif;
241200130716
241300130716           // -?costante "standard" se "DDT no" + Packing-List?
241400161012           When  DB0bam = *blank  and  ($FIAR8 = *on  or  $§PDF = *on);
241500130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
241600130716                     TRASPORTO';
241700130716             // -?senza riserve?
241800130716             if  ILVriserv = *blank;
241900130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
242000130716             // -?con Riserva Consegna?
242100130716             else;
242200130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
242300130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
242400130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
242500130716               endif;
242600130716             endif;
242700130716
242800130716           // -?costante "standard" altrimenti?
242900130716           Other;
243000130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
243100130716             // -?senza riserve?
243200130716             if  ILVriserv = *blank;
243300130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
243400130716             // -?con Riserva Consegna?
243500130716             else;
243600130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
243700130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
243800130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
243900130716               endif;
244000130716             endif;
244100130716
244200130716         EndSl;
244300130716
244400130716         // -?costante aggiuntiva per cliente?
244500130716         //  ?"S G Soluzioni Logistiche"?
244600130716         If  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
244700130716
244800130716           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
244900130716             Pkrm2 = 'INCLUDE CONTRATTO - '
245000130716                   + %trim(DB0nas);
245100130716           endif;
245200130716           if  DB0rmo <> *blank;
245300130716             if  Pkrm2 <> *blank;
245400130716               Pkrm2 = %trimr(Pkrm2)
245500130716                     + ' - MATR TV: '
245600130716                     + %trim(DB0rmo);
245700130716             else;
245800130716               Pkrm2 = 'MATR TV: '
245900130716                     + %trim(DB0rmo);
246000130716             endif;
246100130716           endif;
246200130716
246300130716         EndIf;
246400140108
246500140108         // -?Consegne Parziali Precedenti?
246600140108         If  $ConsParz = *on  and  wB0dcm = *zero;
246700140108
246800140108           Select;
246900140110             When  Pkrm2 = *blank;
247000140110               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
247100140108             When  %subst( Pkrm : 111 ) = *blank;
247200140108               Pkrm = %subst( Pkrm : 1 : 110 ) + ' PARZIALE';
247300140108             When  %subst( Pkrm2 : 111 ) = *blank;
247400140108               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
247500140108           EndSl;
247600140108
247700140108         EndIf;
247800130716
247900130716       ENDSR;
248000090619
248100090619       //--------------------------------------------------------------
248200130716       //?Reperimento Numero e Data del DDT originale del mittente     ?
248300130716       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
248400090619       //--------------------------------------------------------------
248500090619       BEGSR  sr_RepDdtCLN;
248600090619
248700130716         // -?Verifica se la spedizione ha mamma?
248800090619         if  not %open(FNLBL01L);
248900101213           open(e)  FNLBL01L;
249000101213           if  %error;
249100101213             exsr  sr_ErrOpenFile;
249200101213           endif;
249300090619         endif;
249400090619         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
249500090619
249600090619         Select;
249700090619
249800130716           // -?Con Mamma: reperimento dati DDT dalla mamma?
249900090619           When  %found(FNLBL01L);
250000090619             if  not %open(FNBLP01L);
250100101213               open(e)  FNBLP01L;
250200101213               if  %error;
250300101213                 exsr  sr_ErrOpenFile;
250400101213               endif;
250500090619             endif;
250600090619             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
250700090619             if  %found(FNBLP01L);
250800090619               exsr  sr_DatiCLN_blp;
250900090619             endif;
251000090619
251100130716           // -?In Arrivo: reperimento dati DDT da FNARB?
251200090619           Other;
251300090619             exsr  sr_DatiCLN_arb;
251400090619
251500090619         EndSl;
251600090619
251700090619       ENDSR;
251800090619
251900090619       //--------------------------------------------------------------
252000090619       //?In Partenza: reperimento dati DDT da FNBLP                   ?
252100090619       //--------------------------------------------------------------
252200090619       BEGSR  sr_DatiCLN_blp;
252300090619
252400130716         // -?Numero DDT?
252500090619         select;
252600090619           when  BLPrmn <> *zero;
252700100906             if  BLPrmn  > -99999999   and
252800100906                 BLPrmn <   99999999;
252900100906               WddtCLN = BLPrmn;
253000100906             endif;
253100090619           when  BLPncd <> *zero;
253200090619             WddtCLN = BLPncd;
253300090619         endsl;
253400090619
253500130716         // -?Data DDT in formato gg/mm/aaaa?
253600090619         Data_Dmy = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
253700100906         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
253800100906                      ( %subdt(Data_Dmy : *months) * 100 ) +
253900100906                ( %int( %subst( %editc(BLPaas : 'X') : 3 : 2 ) ) ) );
254000090619
254100090619       ENDSR;
254200090619
254300090619       //--------------------------------------------------------------
254400090619       //?In Arrivo: reperimento dati DDT da FNARB                     ?
254500090619       //--------------------------------------------------------------
254600090619       BEGSR  sr_DatiCLN_arb;
254700090619
254800130716         // -?Numero DDT?
254900090619         select;
255000090619           when  ARBrmn <> *zero;
255100100906             if  ARBrmn  > -99999999   and
255200100906                 ARBrmn <   99999999;
255300100906               WddtCLN = ARBrmn;
255400100906             endif;
255500090619           when  ARBncd <> *zero;
255600090619             WddtCLN = ARBncd;
255700090619         endsl;
255800090619
255900130716         // -?Data DDT in formato gg/mm/aaaa?
256000090619         Data_Dmy = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
256100090619         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
256200090619                      ( %subdt(Data_Dmy : *months) * 100 ) +
256300110117                ( %int( %subst( %editc(ARBaas : 'X') : 3 : 2 ) ) ) );
256400090619
256500090619       ENDSR;
256600100517
256700100517       //--------------------------------------------------------------
256800100517       //?Verifica se presente ORM con preavviso mail                  ?
256900100517       //--------------------------------------------------------------
257000110117       BEGSR  sr_CheckORM_PreavvMail;
257100100517
257200130924         //clear  dOrm01;             ?(già fatto)?
257300100517
257400130925         // -?Reperimento num. ORM?
257500130924         exsr  RepAR4_M;
257600100517
257700130925         // -?SE ORM inesistente: inutile proseguire?
257800130925         if  §B4poe = *zero   or   §B4nor = *zero;
257900100517           leavesr;
258000100517         endif;
258100100517
258200130716         // -?Verifica presenza O.R.M.?
258300100517         if  Not  %open(FNORM01L);
258400101213           open(e)  FNORM01L;
258500101213           if  %error;
258600101213             exsr  sr_ErrOpenFile;
258700101213           endif;
258800100517         endif;
258900100517
259000100517         ORMpoe = §B4poe;
259100100517         ORMnsr = §B4nsr;
259200100517         ORMnor = §B4nor;
259300100517         ORMnrv = §B4nrv;
259400100517
259500100517         chain  K04ORM01  FNORM000;
259600100517
259700130716         // -?ORM non reperito: inutile proseguire?
259800100517         if  Not  %found(FNORM01L);
259900100517           leavesr;
260000100517         endif;
260100100517
260200130716         // -?Impostazione flag per stampa porto "ASSEGNATO"?
260300100517         dOrm01 = ORMflo;
260400100517         $PreavvORM = (§ORMpre = 'M');
260500100517
260600100517       ENDSR;
260700090619
260800080430       //--------------------------------------------------------------
260900140303       //?Preparazione dati di output relativi alla Lettera di Vettura ?
261000080430       //--------------------------------------------------------------
261100131016       BEGSR  sr_DatiOUT;
261200080430
261300101221         // -?Impostazione righe?
261400080430         clear LdV;
261500080430         clear wx;
261600080430
261700101221         // -?Area immagine LdV?
261800080801
261900110203         FOR  wx = 1  TO  c_Row;
262000080430
262100080430           %occur(LdV) = wx;
262200080430
262300080508           LdvPrg      = wx;
262400080430           select;
262500080430
262600101221             // ·? 1ª riga?
262700110203             when  wx = 1;
262800110203             LdvTxt = ds_01;
262900080430
263000101221             // ·? 2ª riga (vuota)?
263100110203             when  wx = 2;
263200080430             clear LdvTxt;
263300080430
263400101221             // ·? 3ª riga?
263500110203             when  wx = 3;
263600110203             LdvTxt = ds_03;
263700080430
263800101221             // ·? 4ª riga?
263900110203             when  wx = 4;
264000110203             LdvTxt = ds_04;
264100080430
264200101221             // ·? 5ª riga?
264300110203             when  wx = 5;
264400110203             LdvTxt = ds_05;
264500080430
264600101221             // ·? 6ª riga (vuota)?
264700110203             when  wx = 6;
264800110203             clear LdvTxt;
264900080430
265000101221             // ·? 7ª riga?
265100110203             when  wx = 7;
265200110203             LdvTxt = ds_07;
265300080430
265400101221             // ·? 8ª riga (vuota)?
265500110203             when  wx = 8;
265600080430             clear LdvTxt;
265700080430
265800101221             // ·? 9ª riga?
265900110203             when  wx = 9;
266000110203             LdvTxt = ds_09;
266100080430
266200101221             // ·?10ª riga?
266300110203             when  wx = 10;
266400110203             LdvTxt = ds_10;
266500080430
266600101221             // ·?11ª riga?
266700110203             when  wx = 11;
266800110203             LdvTxt = ds_11;
266900080430
267000101221             // ·?12ª riga?
267100110203             when  wx = 12;
267200110203             LdvTxt = ds_12;
267300080430
267400101221             // ·?13ª riga?
267500110203             when  wx = 13;
267600110203             LdvTxt = ds_13;
267700080430
267800101221             // ·?14ª riga?
267900110203             when  wx = 14;
268000110203             LdvTxt = ds_14;
268100080430
268200101221             // ·?15ª riga?
268300110203             when  wx = 15;
268400110203             LdvTxt = ds_15;
268500080430
268600101221             // ·?16ª riga?
268700110203             when  wx = 16;
268800110203             LdvTxt = ds_16;
268900080430
269000101221             // ·?17ª riga?
269100110203             when  wx = 17;
269200110203             LdvTxt = ds_17;
269300080430
269400101221             // ·?18ª riga?
269500110203             when  wx = 18;
269600110203             LdvTxt = ds_18;
269700080430
269800101221             // ·?19ª riga?
269900110203             when  wx = 19;
270000110203             LdvTxt = ds_19;
270100080430
270200101221             // ·?20ª riga?
270300110203             when  wx = 20;
270400110203             LdvTxt = ds_20;
270500080430
270600101221             // ·?21ª riga?
270700110203             when  wx = 21;
270800110203             LdvTxt = ds_21;
270900080430
271000101221             // ·?22ª riga?
271100110203             when  wx = 22;
271200110203             LdvTxt = ds_22;
271300080430
271400101221             // ·?23ª riga?
271500110203             when  wx = 23;
271600110203             LdvTxt = ds_23;
271700080430
271800101221             // ·?24ª riga?
271900110203             when  wx = 24;
272000110203             LdvTxt = ds_24;
272100080430
272200101221             // ·?25ª riga?
272300110203             when  wx = 25;
272400110203             LdvTxt = ds_25;
272500080430
272600101221             // ·?26ª riga (vuota)?
272700110203             when  wx = 26;
272800080806             clear LdvTxt;
272900080430
273000101221             // ·?27ª riga?
273100110203             when  wx = 27;
273200110203             LdvTxt = ds_27;
273300080430
273400101221             // ·?28ª riga?
273500110203             when  wx = 28;
273600110203             LdvTxt = ds_28;
273700081021
273800101221             // ·?29ª riga?
273900110203             when  wx = 29;
274000081021             LdvTxt = ds_29;
274100081021
274200101221             // ·?30ª riga?
274300110203             when  wx = 30;
274400081021             LdvTxt = ds_30;
274500081021
274600110215             // ·?31ª riga?
274700110203             when  wx = 31;
274800110215             LdvTxt = ds_31;
274900081021
275000101221             // ·?32ª riga?
275100110203             when  wx = 32;
275200081021             LdvTxt = ds_32;
275300081021
275400101221             // ·?33ª riga?
275500110203             when  wx = 33;
275600081021             LdvTxt = ds_33;
275700081021
275800101221             // ·?34ª riga?
275900110203             when  wx = 34;
276000081021             LdvTxt = ds_34;
276100081021
276200101221             // ·?35ª riga (vuota)?
276300110203             when  wx = 35;
276400081021             clear LdvTxt;
276500081021
276600101221             // ·?36ª riga (vuota)?
276700110203             when  wx = 36;
276800081021             clear LdvTxt;
276900080430
277000080430           endsl;
277100080430
277200080430         ENDFOR;
277300080430
277400110518         // -?Impostazione iniziale del modulo standard:?
277500131203         //// ·?Modulo standard con logo Bartolini?
277600131203         //TemplateName = 'ldvbase00 ';
277700131203         //// ·?Modulo standard con logo Bartolini-BRT?
277800131203         //TemplateName = 'ldvbase10 ';
277900120524         // ·?Modulo standard con logo BRT?
278000120524         TemplateName = 'ldvbase20 ';
278100080801
278200110203         Select;
278300081030
278400101221           // -?Area dati "DPD"?
278500101221           //  ?(NON della LNP, ma dalla fil. ricavata dal cod.mittente)?
278600110203           When  §CMntw = 'DPD';
278700110203             exsr  sr_LdVbaseDPD;
278800081030
278900101221           // -?Area dati "FedEx"?
279000150430           When  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
279100110203             exsr  sr_LdVbaseFED;
279200081030
279300131206           // -?Area box (ex "Promemoria per l'autotrasportatore")?
279400131206           When  $1o_Passaggio = *on  or  $ConsParz = *on;
279500110203             exsr  sr_LdVbase01;
279600081030
279700110203         EndSl;
279800130411
279900130411
280000130411         // ? CHIODI PER TEST                               ?
280100130411         Select;
280200130411           // -?Nessun TEST?
280300130411           When  $Test = *blank;
280400130411           // -?Spedizione FedEx?
280500130411           When  $Test = 'FED';
280600130411             TemplateName = 'ldvbase23 ';
280700130411           // -?Spedizione DPD?
280800130411           When  $Test = 'DPD';
280900130411             TemplateName = 'ldvbase22 ';
281000130411           // -?Spedizione con Consegna Parziale Precedente?
281100130411           When  $Test = 'C_P';
281200130411             TemplateName = 'ldvbase21 ';
281300130411           // -?Spedizione Normale?
281400130411           When  $Test = 'NOR';
281500130411             TemplateName = 'ldvbase20 ';
281600130411         EndSl;
281700130411
281800080801
281900101221         // -?Vuoti finali?
282000130213         For  wx = wx  To  w_Row;
282100080801           %occur(LdV) = wx;
282200130213           if  LdVPrg = *zero;
282300130213             LdvPrg = wx;
282400130213             clear  LdvTxt;
282500130213           endif;
282600110203         EndFor;
282700130205
282800130205         // -?Dati aggiuntivi per la 14ª riga (esenzione Iva)?
282900130205         if  DB0ift <> *zero  and
283000130205             DB0iva =  *zero  and
283100130205            (DB0cei <> *blank  or  DB0dce <> *blank);
283200130213           w_Row += 1;
283300130213           wx = w_Row;
283400130205           %occur(LdV) = wx;
283500130205           LdvPrg = 140;
283600130205           LdvTxt = ds_140;
283700130205         endif;
283800130205         // -?Dati aggiuntivi per la 18ª riga (n° fatt. e fil. Iva)?
283900130205         if  DB0nft <> *zero;
284000130213           w_Row += 1;
284100130213           wx = w_Row;
284200130205           %occur(LdV) = wx;
284300130205           LdvPrg = 180;
284400130205           LdvTxt = ds_180;
284500130205         endif;
284600110201
284700110201         // -?Codice a barre: ANNO+LNP+NRS+NSP+CKD:?
284800110201         //  ?richiama il pgm TRUL28R per ricavarne il check digit?
284900110201         reset  TRUL28ds;
285000110201         I28cod = BarCodeDS;
285100110201         TRUL28R1 ( TRUL28ds );
285200110201
285300110201         // -?Impostazione campo "LastRow"?
285400110201         LastRow = 'Spedizione: ' + %subst( O28cod : 1 : 15 ) + '  ' +
285500110203                   'Distinta: ' + %subst( ILVprmDst : 1 : 3 ) + '-'
285600170707                                + %subst( ILVprmDst : 4 ) +
285700170707                   '  (' + dILVlibero.§ILVapl + ')';
285800080801
285900110203         // ______________________________
286000110203         //?Stampa di prova (SE richiesta)?
286100110203         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
286200110519         If  SDSprm > 11;
286300130716           exsr  sr_Print;
286400130716         EndIf;
286500080430
286600080430       ENDSR;
286700130716
286800130716       //--------------------------------------------------------------
286900130716       //?Stampa dei dati per l'immagine - SE RICHIAMATO PER PROVA     ?
287000130716       //--------------------------------------------------------------
287100130716       BEGSR  sr_Print;
287200130716
287300130716         // -?Apertura file di stampa?
287400130716         if not %open(QSYSPRT);
287500130716           open  QSYSPRT;
287600130716         endif;
287700130716
287800130716         // -?Stampa "testata"?
287900130716         except PRTtxt;
288000130716
288100130716         // -?Stampa dettaglio?
288200130716         for wx = 1 to w_Row;
288300130716           %occur(LdV) = wx;
288400130716           except PRTrow;
288500130716         endfor;
288600130716
288700130716         // -?Stampa "ultima riga"?
288800130716         except PRTend;
288900130716
289000130716         // -?Chiusura file di stampa?
289100130716         close QSYSPRT;
289200130716
289300130716       ENDSR;
289400081030
289500081030       //--------------------------------------------------------------
289600130716       //?Preparazione dati x spedizione "DPD"                         ?
289700081030       //--------------------------------------------------------------
289800130716       BEGSR  sr_LdVbaseDPD;
289900081030
290000120524         // -?Impostazione del modulo DPD:?
290100131203         //// ·?Modulo DPD con logo Bartolini?
290200131203         //TemplateName = 'ldvbase02 ';
290300131203         //// ·?Modulo DPD con logo Bartolini/BRT?
290400131203         //TemplateName = 'ldvbase12 ';
290500120524         // ·?Modulo DPD con logo BRT?
290600120524         TemplateName = 'ldvbase22 ';
290700081030
290800101221         // -?Impostazione data?
290900081030         %occur(LdV) = 19;
291000081030         %subst(LdVTxt : 94) = 'Date ' + PdspA;
291100081030
291200101221         // -?Impostazione numero colli?
291300081030         %occur(LdV) = 20;
291400081030         %subst(LdVTxt : 94) = 'Parcel N.' + PnclA;
291500081030
291600101221         // -?Impostazione numero parcel?
291700081030         %occur(LdV) = 21;
291800171117         %subst(LdVTxt : 94) = PNDipn;
291900081111
292000131015         // -?"Decodifica" del barcode?
292100081111         %occur(LdV) = 22;
292200081111         %subst(LdVTxt : 94) = *blank;
292300171117         if  PNDipn <> *blank;
292400101221           // -?controllo se vecchio o nuovo barcode?
292500171117           if  %subst(PNDipn : 14 : 1) = *blank;
292600081111             // - - vecchio
292700171117             %subst(LdVTxt : 94) = '01 ' + %subst(PNDipn : 1 : 3)
292800171117                                 + ' '   + %subst(PNDipn : 4 : 8);
292900081111           else;
293000081111             // - - nuovo
293100171117             %subst(LdVTxT : 94) = '01 ' + %subst(PNDipn : 1 :  4)
293200171117                                 + ' '   + %subst(PNDipn : 5 : 10);
293300081111           endif;
293400081111         endif;
293500081111
293600130716         // -?Impostazione costanti  (LB ha detto di NON metterle)?
293700081111         //%occur(LdV) = 23;
293800081111         //%subst(LdVTxt : 94) = *blank;
293900081111         //%occur(LdV) = 24;
294000081111         //%subst(LdVTxt : 94) = 'Controllare la corrispon-';
294100081111         //%occur(LdV) = 25;
294200081111         //%subst(LdVTxt : 94) = 'denza del destinatario con';
294300081111         //%occur(LdV) = 26;
294400081111         //%subst(LdVTxt : 94) = 'l''indirizzo dell''etichetta';
294500081111         //%occur(LdV) = 27;
294600081111         //%subst(LdVTxt : 94) = 'DPD.';
294700081030
294800081030       ENDSR;
294900081030
295000081030       //--------------------------------------------------------------
295100130716       //?Preparazione dati x spedizione "FedEx"                       ?
295200081030       //--------------------------------------------------------------
295300130716       BEGSR  sr_LdVbaseFED;
295400081030
295500120524         // -?Impostazione del modulo FedEx:?
295600131203         //// ·?Modulo FedEx con logo Bartolini?
295700131203         //TemplateName = 'ldvbase03 ';
295800131203         //// ·?Modulo FedEx con logo Bartolini/BRT?
295900131203         //TemplateName = 'ldvbase13 ';
296000120524         // ·?Modulo FedEx con logo BRT?
296100120524         TemplateName = 'ldvbase23 ';
296200110517
296300081103         w_Row  = c_Row_B;
296400081030         Wtrc = 'C';
296500081030
296600101221         // -?Preparazione elenco segnacolli FedEx?
296700101213         if  NOT %open(FNART01L);
296800101213           open(e)  FNART01L;
296900101213           if  %error;
297000101213             exsr  sr_ErrOpenFile;
297100101213           endif;
297200101213         endif;
297300101213         if  NOT %open(FIARS01L);
297400101213           open(e)  FIARS01L;
297500101213           if  %error;
297600101213             exsr  sr_ErrOpenFile;
297700101213           endif;
297800101213         endif;
297900081030
298000101221         // -?Impostazione numeratore fino alla 39ª riga?
298100101213         FOR  wx = wx  TO (w_Row - c_Row_Fed);
298200081030
298300081030           %occur(LdV) = wx;
298400081030           LdvPrg      = wx;
298500081030
298600101213         ENDFOR;
298700081030
298800101221         // -?40ª riga & successive?
298900081030         clear  xx;
299000081030         clear  Wnsc;
299100101221         FOR  wx = (w_Row - c_Row_Fed + 1)  TO w_Row;
299200081030
299300081030           %occur(LdV) = wx;
299400081030           LdvPrg      = wx;
299500081030
299600081030           // - Caricamento AWB FedEx
299700081103           clear  wAWB;
299800101221           // - -?leggo segnacolli per bolle in arrivo?
299900081030           exsr  CarAWB_Arx;
300000081030           if  wAWB = *blank;
300100081030             leave;
300200081030           endif;
300300081030
300400081030           xx    += 1;
300500081103           Pptn   = wAWB;
300600081030           Psts   = %editc( xx : 'Z' );
300700081030           LdvTxt = ds_Fed;
300800081030
300900101221         ENDFOR;
301000081030
301100081030       ENDSR;
301200081030
301300081030       //--------------------------------------------------------------
301400130716       //?Riempimento del box "Promemoria per l'autotrasportore"       ?
301500081030       //--------------------------------------------------------------
301600130716       BEGSR  sr_LdVbase01;
301700081030
301800120524         // -?Impostazione del modulo con Box aggiuntivo (14 rec. in +)?
301900131203         //// ·?Modulo Box aggiuntivo con logo Bartolini?
302000131203         //TemplateName = 'ldvbase01 ';
302100131203         //// ·?Modulo Box aggiuntivo con logo Bartolini/BRT?
302200131203         //TemplateName = 'ldvbase11 ';
302300120524         // ·?Modulo Box aggiuntivo con logo BRT?
302400120524         TemplateName = 'ldvbase21 ';
302500110517
302600081103         w_Row  = c_Row_B;
302700081030
302800101221         // -?Intestazione box "Promemoria per l'autotrasportatore"?
302900131206         //  ?(NON LA SI FA PIÙ)?
303000131206         //exsr  sr_IntestBox;
303100131206
303200131206
303300131206         // -?Posizionamento sulla 36ª riga?
303400131206         //  ?(Il ciclo "FOR" precedente dovrebbe aver già posizionato?
303500131206         //  ? sulla 37ª riga)?
303600131206         wx = 36;
303700131206
303800131206         // -?Se reperito il 1° PASSAGGIO (c/o destinatario):?
303900131206         IF  $1o_Passaggio;
304000131206
304100131206           test(de) EVBdev;
304200131206           if %error;
304300131206             clear w0080;
304400131206           else;
304500131206             Data_Eur = %date( %char(EVBdev) : *iso0 );
304600131206             w0080    = %dec(Data_Eur);
304700131206           endif;
304800131206
304900131206           // -?37ª riga?
305000131206           wx += 1;
305100131206           %occur(LdV) = wx;
305200131206           LdvPrg      = wx;
305300131206           Ppma    = 'Primo passaggio del '
305400131206                   + %editc( w0080 : 'Y' )
305500131206                   + ' effettuato alle '
305600131206                   + %editw( EVBhev: '  :  ' );
305700131206           LdvTxt  = ds_37;
305800131206
305900131206           // -?38ª riga: VUOTA (separatrice)?
306000131206           wx += 1;
306100131206           %occur(LdV) = wx;
306200131206           LdvPrg      = wx;
306300131206           clear  LdvTxt;
306400131206
306500131206         ENDIF;
306600131206
306700081030
306800131206         // -?Se reperite CONSEGNE PARZIALI PRECEDENTI:?
306900131206         IF  $ConsParz = *on;
307000131206
307100131206           // -?Impostazione segnalazione in output?
307200131206           xx = 1;
307300131206           DoW  (xx <= %elem($CP)   and   $CP(xx) < *all'9');
307400131206
307500131206             Elem_CP = $CP(xx);
307600131206             test(de) WCPdcm;
307700131206             if %error;
307800131206               clear w0080;
307900131206             else;
308000131206               Data_Eur = %date( %char(WCPdcm) : *iso0 );
308100131206               w0080    = %dec(Data_Eur);
308200131206             endif;
308300131206
308400131206             // -?37ª o 39ª riga & successive?
308500131206             wx += 1;
308600131206             %occur(LdV) = wx;
308700131206             LdvPrg      = wx;
308800131206             Ppma    = 'Consegnati colli '
308900131206                     + %editc(WCPncl:'Z')
309000131206                     + ' in data '
309100140304                     + %editc(w0080:'Y');
309200140304             if  WCPocm > *zero;
309300140304               Ppma    = %trimr( Ppma )
309400140304                       + ' alle ore '
309500140304                       + %editw( WCPocm : '0 :  ' );
309600140304             endif;
309700140304             if  WCPfir <> *blank;
309800140304               Ppma    = %trimr( Ppma )
309900140303                     + ' - Firmatario: '
310000140303                     + WCPfir;
310100140304             endif;
310200131206             LdvTxt  = ds_37;
310300131206
310400131206             xx += 1;
310500131206
310600131206           EndDo;
310700131206
310800131206         ENDIF;
310900081030
311000081030       ENDSR;
311100080801
311200080801       //--------------------------------------------------------------
311300130716       //?Intestazione del box "Promemoria per l'autotrasportore"      ?
311400080801       //--------------------------------------------------------------
311500131206       //BEGSR  sr_IntestBox;
311600131206       //
311700131206       //  // 37ª riga
311800131206       //  //wx += 1;         (già incrementato dal ciclo "FOR" precedente)?
311900131206       //  %occur(LdV) = wx;
312000131206       //  LdvPrg      = wx;
312100131206       //  Ppma    = '                             '
312200131206       //          + c_PMxA;
312300131206       //  LdvTxt  = ds_37;
312400131206       //
312500131206       //  // -?38ª riga?
312600131206       //  wx += 1;
312700131206       //  %occur(LdV) = wx;
312800131206       //  LdvPrg      = wx;
312900131206       //  Ppma    = *all'=';
313000131206       //  LdvTxt  = ds_37;
313100131206       //
313200131206       //ENDSR;
313300101221
313400080430      /end-free
313500081030      *
313600081030      *---------------------------------------------------------------*
313700081030      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
313800081030      *---------------------------------------------------------------*
313900081030     c     CarAWB_Arx    BEGSR
314000130215      *
314100130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
314200130215      *   comunque una bolla mamma
314300130215     c                   exsr      sr_Mamma_FedEx
314400081030      *
314500081030      * Posizionamento
314600130215if  2c                   if        $Mamma = *off   and
314700130215     c                             $Mamma_FedEx = *off
314800081030     c     KLDVnsc       setgt     FNART000
314900081030x   2c                   else
315000081030     c     KLBLnsc       setgt     FNART000
315100081030e   2c                   endif
315200081030      *
315300081030      * Ciclo di lettura
315400081030do  1c                   DO        *hival
315500081030      *
315600130215if  2c                   if        $Mamma = *off   and
315700130215     c                             $Mamma_FedEx = *off
315800081030     c     KLDV          reade     FNART000
315900081030x   2c                   else
316000081030     c     KLBL          reade     FNART000
316100081030e   2c                   endif
316200081030if  2c                   if        %eof(FNART01L)
316300081030     c                   leave
316400081030e   2c                   endif
316500081030      * - per ogni segnacollo cerco l'AWB
316600081030     c     K05ARS01A     chain     FIARS000
316700081030if  2c                   if            %found(FIARS01L)
316800081030     c                             and %subst(ARSnot:1:1) >= *zeros
316900081030     c                   eval      wAWB = %subst(ARSnot:1:12)
317000081030     c                   eval      wNSC = ARSnsc
317100081030     c                   leave
317200081030e   2c                   endif
317300081030      *
317400081030e   1c                   ENDDO
317500081030      *
317600081030     c                   ENDSR
317700130215      *
317800130215      /free
317900130215
318000130215       //--------------------------------------------------------------
318100130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
318200130215       //--------------------------------------------------------------
318300130215       BEGSR  sr_Mamma_FedEx;
318400130215
318500130924         //$Mamma_FedEx = *off;         ?(già così)?
318600130215
318700130924         If  Not $Mamma;
318800130215
318900130215           // -?Verifica se la spedizione ha comunque mamma?
319000130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
319100130215           if  not %open(FNLBL01L);
319200130215             open  FNLBL01L;
319300130215           endif;
319400130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
319500130215
319600130215           $Mamma_FedEx = %found(FNLBL01L);
319700130215
319800130215         EndIf;
319900130215
320000130215       ENDSR;
320100130215
320200130215      /end-free
320300131008      *
320400131008      *---------------------------------------------------------------*
320500131008      *?Prepara elenco SegnaColli Non Sequenziali (dett.) - MAX 2 -  ?*
320600131008      *?  da FNART (arrivo)                                          ?*
320700131008      *---------------------------------------------------------------*
320800131008     c     ListaScNSd    BEGSR
320900131008      *
321000131008      * SE ci sono solo 2 segnacolli non sequenziali:
321100131008      *   staranno al posto del range - basta non indicare
321200131008      *   "NON SEQUENZIALI"
321300131008      *
321400131008if  1c                   if        not %open(FNART01L)
321500131008     c                   open(e)   FNART01L
321600131008      /free
321700131008           if  %error;
321800131008             exsr  sr_ErrOpenFile;
321900131008           endif;
322000131008      /end-free
322100131008e   1c                   endif
322200131008      *
322300131008      * memorizzo l'ultimo numero di segnacollo
322400131008if  1c                   if        $Mamma = *off
322500131008     c     KLDV          setgt     FNART000
322600131008     c     KLDV          readpe    FNART000
322700131008x   1c                   else
322800131008     c     KLBL          setgt     FNART000
322900131008     c     KLBL          readpe    FNART000
323000131008e   1c                   endif
323100131008if  1c                   if        NOT %eof(FNART01L)
323200131008     c                   eval      %subst(Pnsx : 8) = ' , '
323300131008     c                                              + %editc(ARTnsc : 'Z')
323400131008e   1c                   endif
323500131008      *
323600131008      * memorizzo il primo numero di segnacollo
323700131008if  1c                   if        $Mamma = *off
323800131008     c     KLDV          setll     FNART000
323900131008     c     KLDV          reade     FNART000
324000131008x   1c                   else
324100131008     c     KLBL          setll     FNART000
324200131008     c     KLBL          reade     FNART000
324300131008e   1c                   endif
324400131008if  1c                   if        NOT %eof(FNART01L)
324500131008     c                   eval      %subst(Pnsx : 1 : 7) =
324600131008     c                                 %editc(ARTnsc : 'Z')
324700131008e   1c                   endif
324800131008      *
324900131008     c                   ENDSR
325000030515      *
325100030515      *---------------------------------------------------------------*
325200050217      *?Carico LDV da FNARB00F                                       ?*
325300030515      *---------------------------------------------------------------*
325400110208     c     sr_CarARB     BEGSR
325500030515      *
325600110202      /free
325700110202
325800110202         //____________________________________________________________
325900110202         //? RISULTATO è impostato di default a "OK" => creo immagine ?
326000110202         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
326100110202
326200110202         // »? STEP 1 ?«
326300131203         //  ?Verifica se presenti forzature per la distinta           ?
326400131203         //  ?(vedi file FIIRD00F)                                     ?
326500131203
326600131203         //?-?Verifica se distinta in FIIRD10F                           ?
326700131203         //  ?e se elaborabile con (IRDTPELAB = *blank) o senza controlli?
326800130716         chain  %kds(k01fiird11)  FIIRD000;
326900130313
327000130716         If  %found(FIIRD11L)  and  IRDtpElab = *blank;
327100131203           // -?SE rileva errori =>?
327200131203           // -?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
327300130716           exsr  sr_STEP_1_CtrlTASFLO;
327400130716         EndIf;
327500131203
327600131203         // »? STEP 2 ?«
327700131203         //  ?Verifica se spedizione elaborabile                       ?
327800131203         //  ?(controlli su distinta e spedizione nel file FIILV00F)   ?
327900131203         //  ?- SE distinta SENZA forzature in FIIRD00F -              ?
328000110202
328100110204         // -?Reperimento dati distinta (FIDST)?
328200110228         clear  dDstflr;
328300110202         k_DSTnpg = 4;
328400110202         k_DSTnfv = ds_Distinta.p_Ndc;
328500110202         k_DSTfgs = ds_Distinta.p_Fgs;
328600110202         chain  %kds(k03fidst01)  FIDST000;
328700110228         if  %found(FIDST01L);
328800110228           dDSTflr = DSTflr;
328900110228         endif;
329000110202
329100110202         // -?Controllo distinta:?
329200130920         //?·?(SE Distinta NON trovata in FIIRD10F per recupero immagini?
329300130924         //  ?  & NON è una spedizione FORZATA PER TEST)                ?
329400131016         //  ?SE distinta NON trovata in FIDST01L: registra LOG e prosegue?
329500131016         //  ?SE distinta NON chiusa:?
329600131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
329700130716         If  Not %found(FIIRD11L)  and
329800130716             $Test = *blank;
329900131203           exsr  sr_STEP_2_CtrlDISTINTA;
330000130716         EndIf;
330100110202
330200110202
330300110202         // -?Reperimento dati Immagine LdV (FIILV)?
330400110208         clear  ILVcol;
330500110309         clear  wCountRec01;
330600110202         chain(e)  %kds(k06fiilv01)  FIILV000;
330700130313
330800130716         //?·?DISTINTA in FIIRD10F => recupero immagine (senza ctrl)?
330900131016         //?·?DISTINTA NON in FIIRD10F => controlli da eseguire     ?
331000131016         //  ?SE trovati errori:?
331100131016         //  ?"risponde" ed eventualmente esce dalla s.p. (direttamente dalla subr.)?
331200130716         If  Not  %found(FIIRD11L);
331300131203           exsr  sr_STEP_2_CtrlFIILV;
331400130716         EndIf;
331500130313
331600131203         // »? STEP 3 ?«
331700131203         //  ?Controllo spedizione                                     ?
331800131203         //  ?(da eseguire comunque)                                   ?
331900110202
332000131203         // -?Reperimento dati Spedizione (FNARB)?
332100110207         chain  KLDV  FNARB000;
332200101220
332300110202         // -?Spedizione non trovata?
332400101221         if  NOT %found(FNARB01L);
332500130924           // ·?Spedizione non trovata => ERRORE: non rielaborare immagine?
332600131203           //  ?"risponde" ed esce dalla s.p.?
332700110204           Risultato = 'XX';
332800130924           //RtnOpCode = 'FNARB_MANC';
332900130924           //RtnEsito  = -1;
333000130924           // -?Registrazione log in FIILV, differenziando l'errore per?
333100130924           //  ?iter normale (25) o iter con forzatura (52)?
333200130924           if  Not  %found(FIIRD11L);
333300130313             wCod_Log = '25';
333400130716           else;
333500130716             wCod_Log = '52';
333600130716           endif;
333700110208           exsr  sr_WrtFIILV;
333800101221           // -?Uscita dalla s.p.?
333900110204           exsr  sr_End;
334000101221         endif;
334100101221
334200101221
334300110202         // -?Registrazione record per spedizione "OK" (root "buono")?
334400110218         If  wCod_Log = *blank;
334500130206           RootFolder = §ILVimgOk;
334600110203           wCod_Log = '01';
334700110202           exsr  sr_WrtFIILV;
334800110218         EndIf;
334900090121
335000080523      /end-free
335100070514      *
335200070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
335300070514      *   il reperimento dell'eventuale bolla mamma...
335400070514     c                   eval      wB0ntc =  ARBntc
335500120524     c                   eval      wB0dcp =  ARBdcp
335600120524     c                   eval      wB0tmc =  ARBtmc
335700120524     c                   move      ARBpdc        Wpdc4a
335800080523     c                   eval      wB0dcm =  ARBdcm
335900121018     c                   eval      wB0piv =  ARBcpi
336000030515      *
336100030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
336200030515     c                   exsr      sr_DS_ARB
336300030515      *
336400070308      * Decodifico filiale di Arrivo
336500131203     c                   exsr      sr_DecLNA
336600030515      *
336700030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
336800030515      *    "generiche" (3A, TB, 5E)
336900030515     c                   exsr      sr_DS_TAB
337000030515      *
337100030515      * Ultimi dati mancanti:
337200030515      * - Cod. cliente mittente
337300030515if  1c                   If        ARBccm <> *zeros
337400030515     c                   movel     ARBccm        DB0ccm
337500030515x   1c                   Else
337600030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
337700030515     c                   movel     ARBksc        DB0ccm
337800030515e   2c                   endif
337900030515e   1c                   EndIf
338000030515      * - Cod. cliente destinatario
338100030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
338200030515     c                   movel     ARBksc        DB0ccd
338300030515e   1c                   endif
338400030515      * - Se c'è 2ª bolla imposto LNA9999
338500030515if  1c                   if        DB0tb2 <> *blanks
338600030515     c                   eval      DB0ccd = *all'9'
338700030515     c                   movel     ARBlna        DB0ccd
338800030515e   1c                   endif
338900120524      * - Reperimento Data spedizione
339000120524if  1c                   if        DB0mgs <> *zeros
339100120524     c                   z-add     DB0mgs        Wdsp
339200120524     c                   movel     DB0aas        Wdsp
339300120524x   1c                   else
339400120524      *   prendo la data del giorno se non c'è la data spedizione
339500120524     c                   time                    w0140
339600120524     c                   move      w0140         w0080
339700120524     c     *eur          movel     w0080         Data_EUR
339800120524     c     *iso          movel     Data_EUR      Wdsp
339900120524e   1c                   endif
340000030515      *
340100061030      * 3ª impostaz. campi della DS: importi da calcolare
340200061031     c                   exsr      sr_DS_IMPO
340300030515      *
340400080505     c                   ENDSR
340500080724      /free
340600130716
340700130716       //--------------------------------------------------------------
340800130716       //?»  STEP 1  «                                                 ?
340900130716       //?Distinta in FIIRD10F e senza forzature (IRDTPLAB = *blank):  ?
341000130716       //?verifica se immagine già generata (DTASFLO.§FLOIML <> *blank)?
341100130716       //--------------------------------------------------------------
341200130716       BEGSR  sr_STEP_1_CtrlTASFLO;
341300130716
341400130716         // -?Reperimento spedizione da TITAS per verificarne lo stato?
341500131016         //  ?della relativa immagine.  Se già presente immagine:?
341600131016         //  ?"risponde" con codice errore 51 e "non elaborare"?
341700131016         //  ?ed esce dalla s.p.?
341800130716         if  %subst( wSysName : 1 : 6 ) = 'SETRAS';
341900130716           wLibFileTITAS = 'GAITRAGRU/TITAS30C';
342000130716         else;
342100130716           wLibFileTITAS = 'GAITRAGRPS/TITAS30C';
342200130716         endif;
342300130716         if  NOT  %open(TITAS30C);
342400130716           open(e)  TITAS30C;
342500130716           if  %error;
342600130716             exsr  sr_ErrOpenFile;
342700130716           endif;
342800130716         endif;
342900130716
343000130716         chain  %kds(k05titas30 : 4)  TITAS30C;
343100130716
343200130716         If  %found(TITAS30C)            and
343300130716             TASdcm > *zero              and
343400130716             TASndc = ds_Distinta.p_Ndc  and
343500130716             %int( TASpdc / 10000 ) = ds_Distinta.p_Fgs;
343600130716
343700130716           dTASflo = TASflo;
343800130716
343900130716           if  dTASflo.§FLOiml <> *blank;
344000130716             Risultato = 'XX';
344100130924             //RtnOpCode = 'GIÀ_IMMAG ';
344200130924             //RtnEsito  = -1;
344300130716             // -?Registrazione log in FIILV?
344400130716             wCod_Log = '51';
344500130716             exsr  sr_WrtFIILV;
344600130716             // -?Uscita dalla s.p.?
344700130716             exsr  sr_End;
344800130716           endif;
344900130716
345000130716         EndIf;
345100130716
345200130716       ENDSR;
345300130716
345400130716       //--------------------------------------------------------------
345500131203       //?»  STEP 2  «                                                 ?
345600131203       //?Distinta NON in FIIRD10F:?controlli sulla distinta.          ?
345700130716       //--------------------------------------------------------------
345800131203       BEGSR  sr_STEP_2_CtrlDISTINTA;
345900130716
346000130716         Select;
346100130716
346200130716           // ·?DISTINTA non trovata => ANOMALIA?
346300130716           When  NOT  %found(FIDST01L)  or
346400130716                 DSTatb <> *blank;
346500130924             // -?Registrazione log in FIILV - se non già fatto?
346600130716             if  wCod_Log = *blank;
346700130716               wCod_Log = '22';
346800130716               exsr  sr_WrtFIILV;
346900130716             endif;
347000130716
347100131016           // ·?DISTINTA non chiusa => ripetere la richiesta?
347200130716           When  DSTfcf = *blank;
347300130716             Risultato  = 'NO';
347400130924             //RtnOpCode  = 'DIST_APERT';
347500130716             //RtnEsito   = -1;
347600130716             // -?Uscita dalla s.p.?
347700130716             exsr  sr_End;
347800130716
347900130716         EndSl;
348000130716
348100130716       ENDSR;
348200130716
348300130716       //--------------------------------------------------------------
348400131203       //?»  STEP 2  «                                                 ?
348500131203       //?Distinta NON in FIIRD10F:?controlli sul/i record in FIILV00F.?
348600130716       //--------------------------------------------------------------
348700131203       BEGSR  sr_STEP_2_CtrlFIILV;
348800130716
348900130716         Select;
349000130716
349100130716           // ·?Rec. IMMAGINE LdV allocato => ripetere la richiesta?
349200130716           When  %error;
349300130716             Risultato  = 'NO';
349400130924             //RtnOpCode  = 'FIILV_ALLC';
349500130716             //RtnEsito   = -1;
349600130716             // -?Uscita dalla s.p.?
349700130716             exsr  sr_End;
349800130716
349900130716           // ·?Rec. IMMAGINE LdV non trovato?
350000130716           When  NOT  %found(FIILV01L);
350100130716             select;
350200130716               // ·?DISTINTA senza scarico a PDA delle spedizioni?
350300130716               //  ?=> cancellare immagine (senza LOG)?
350400130716               when  DSTpda <> 'C'   and   DSTpda <> 'E';
350500130716                 Risultato  = 'XX';
350600130924                 //RtnOpCode  = 'DIST_NOPDA';
350700130716                 //RtnEsito   = -1;
350800130716                 // -?Uscita dalla s.p.?
350900130716                 exsr  sr_End;
351000130716               // ·?DISTINTA in test consegne?
351100130716               //  ?=> cancellare immagine (senza LOG)?
351200130716               when  §DSTtstPda = 'C'   or   §DSTtstPda = 'E';
351300130716                 Risultato  = 'XX';
351400130924                 //RtnOpCode  = 'DIST_TEST ';
351500130716                 //RtnEsito   = -1;
351600130716                 // -?Uscita dalla s.p.?
351700130716                 exsr  sr_End;
351800130716               // ·?Altrimenti: ANOMALIA?
351900130924               //  ?Registrazione log in FIILV - se non già fatto?
352000130716               other;
352100130716                 if  wCod_Log = *blank;
352200130716                   wCod_Log = '23';
352300130716                   exsr  sr_WrtFIILV;
352400130716                 endif;
352500130716             endsl;
352600130716
352700130716           // ·?Rec. IMMAGINE LdV già elaborato?
352800130716           //  ?=> ricerca record NON elaborato (ILVCOL = "00")?
352900130716           When  ILVcol <> '00';
353000130716             exsr  sr_CercaLog00;
353100130716             // ·?Rec. IMMAGINE LdV non trovato (né "00" né "01")?
353200130924             //  ?Registrazione log in FIILV - se non già fatto?
353300130716             if  ILVcol <> '00'  and  wCountRec01 = *zero
353400130716                                 and  wCod_Log    = *blank;
353500130716               wCod_Log = '23';
353600130716               exsr  sr_WrtFIILV;
353700130716             endif;
353800130716
353900130716         EndSl;
354000130716
354100130716
354200130716         // -?Verifica SE trovato record da elaborare per la sped/dist?
354300130716         $FIILV = (ILVcol = '00');
354400130716
354500130716         // -?Impostazione struttura dati del campo ILVLIBERO?
354600130716         dILVlibero = ILVlibero;
354700130716
354800130716
354900130716         Select;
355000130716
355100130716           // -?Spedizione non consegnata in distinta?
355200130716           When  ILVnoCDst = 'S';
355300130924             // -?Registrazione log in FIILV - se non già fatto?
355400130716             if  wCod_Log = *blank;
355500130716               wCod_Log = '08';
355600130716               exsr  sr_WrtFIILV;
355700130716             endif;
355800130716
355900130716           // -?LdV da scannerizzare?
356000130716           When  ILVscan = *blank;
356100130924             // -?Registrazione log in FIILV - se non già fatto?
356200130716             if  wCod_Log = *blank;
356300130716               wCod_Log = '09';
356400130716               exsr  sr_WrtFIILV;
356500130716             endif;
356600130716
356700130716         EndSl;
356800130716
356900130716       ENDSR;
357000130716
357100130716       //--------------------------------------------------------------
357200130716       //?Ricerca record IMMAGINE LDV NON elaborato (ILVCOL = "00")    ?
357300130716       //--------------------------------------------------------------
357400130716       BEGSR  sr_CercaLog00;
357500130716
357600130716         //?N.B.?Il 1° rec. per spedizione/distinta è già stato letto?
357700130716
357800130716         DoU  ILVcol = '00';
357900130716
358000130716           // ·?Rec. Immagine LdV già elaborato ("01")?
358100130716           if  ILVcol = '01';
358200130716             wCountRec01 += 1;
358300130716           endif;
358400130716
358500130716           reade(e)  %kds(k06fiilv01)  FIILV000;
358600130716
358700130716           select;
358800130716
358900130716             // ·?Rec. Immagine LdV allocato => ripetere la richiesta?
359000130716             when  %error;
359100130716               Risultato  = 'NO';
359200130924               //RtnOpCode  = 'FIILV_ALLC';
359300130716               //RtnEsito   = -1;
359400130716               // -?Uscita dalla s.p.?
359500130716               exsr  sr_End;
359600130716
359700130716             // ·?Solo rec. Immagine LdV già elaborati?
359800130716             //  ?=> scrittura nuovo?
359900130716             when  %eof(FIILV01L);
360000130716               clear  ILVcol;
360100130716               leave;
360200130716
360300130716             // ·?Già reperiti 100 rec. per le stesse sped./dist.?
360400130716             //  ?(questo è in realtà il 101°)?
360500131016             //  ?=> NON ripetere la richiesta (senza LOG)?
360600130716             when  wCountRec01 >= 100;
360700130716               Risultato  = 'XX';
360800130924               //RtnOpCode  = 'SPEDGIÀ100';
360900130716               //RtnEsito   = -1;
361000130716               // -?Invio e-mail?
361100130716               exsr  sr_SendMAIL;
361200130716               // -?Uscita dalla s.p.?
361300130716               exsr  sr_End;
361400130716
361500130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
361600130716             //  ?creazione immagine sul PDA successive all'attuale?
361700131016             //  ?=> NON ripetere la richiesta (senza LOG)?
361800130716             //when  DataOraCrea <= ILVprmTimP;
361900130716             when  DataOraCrea < ILVprmTimP;
362000130716               Risultato  = 'XX';
362100130924               //RtnOpCode  = 'SPED_GIÀEL';
362200130716               //RtnEsito   = -1;
362300130716               // -?Uscita dalla s.p.?
362400130716               exsr  sr_End;
362500130716
362600130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
362700130716             //  ?creazione immagine sul PDA identica all'attuale?
362800130716             //  ?=> riprovare dopo 10 minuti (senza LOG)?
362900130716             when  DataOraCrea = ILVprmTimP  and
363000130716                   ILVtimA > %int( %subst( %char( %dec(
363100130716                                   %timestamp() - %minutes(10) ) )
363200130716                                           : 1 : 14 ) );
363300130716               Risultato  = 'NO';
363400130924               //RtnOpCode  = '..TRA10MIN';
363500130716               //RtnEsito   = -1;
363600130716               // -?Uscita dalla s.p.?
363700130716               exsr  sr_End;
363800130716
363900130716           endsl;
364000130716
364100130716         EndDo;
364200130716
364300130716       ENDSR;
364400101221
364500080724       //--------------------------------------------------------------
364600130716       //?Registrazione record nel work-file FIILV00F                  ?
364700080724       //--------------------------------------------------------------
364800110207       BEGSR  sr_WrtFIILV;
364900110202
365000110202         // -?Segnalazione anomalia rilevata per la spedizione?
365100110204         if  Not  $FIILV;
365200110202           clear  FIILV000;
365300110202           ILVfgs = ds_Distinta.p_Fgs;
365400110202           ILVndc = ds_Distinta.p_Ndc;
365500110202           ILVaas = DB0aas;
365600110202           ILVlnp = DB0lnp;
365700110202           ILVnrs = DB0nrs;
365800110202           ILVnsp = DB0nsp;
365900110202         endif;
366000110202
366100110207         ILVcol     = wCod_Log;
366200110202         xx = %lookup( wCod_Log : $CoL );
366300110202         if  xx > *zero;
366400110202           ILVnot   = $DeL(xx);
366500110202         endif;
366600110202         ILVprmSpe  = Spedizione;
366700110202         ILVprmDst  = Distinta;
366800110202         ILVprmTimp = DataOraCrea;
366900110202         ILVrisult  = Risultato;
367000110202         ILVtimA    = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
367100170707
367200170707         dILVlibero.§ILVapl = 'D';
367300170707
367400170707         ILVlibero = dILVlibero;
367500130411
367600130411
367700131203         // -? Spedizioni FORZATE PER TEST => NON scrive il rec.       ?
367800130411         IF  $Test <> *blank;
367900130411           leavesr;
368000130411         ENDIF;
368100130411
368200090210
368300110923
368400131203         // -?S.P. richiamata per stampa cartacea => NON scrive il rec.?
368500110923         If  SDSprm > 11;
368600110923           leavesr;
368700110923         EndIf;
368800110923
368900110923
369000110204         if  $FIILV;
369100110202           //_______________
369200110202           update  FIILV000;
369300110202           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
369400110202         else;
369500110204           //_______________
369600110204           write   FIILV000;
369700110204           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
369800110202         endif;
369900080724
370000080724       ENDSR;
370100130227
370200130227       //--------------------------------------------------------------
370300130716       //?Invio e-mail per avvisare degli oltre 100 rec. con cod. LOG  ?
370400130716       //?   "01" per la stessa spedizione in FIILV.                   ?
370500130227       //--------------------------------------------------------------
370600130227       BEGSR  sr_SendMAIL;
370700130227
370800130227         // -?Reperimento tab. "MRA"?
370900130227         reset  dMRAdan;
371000130227         reset  TIBS02ds;
371100130227         T02cod = 'MRA';
371200130227         T02ke1 = SDSpgm;
371300130227         T02ke2 = *blank;
371400130227         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
371500130227         if  T02err = *blank;
371600130227           dMRAdan = T02uni;
371700130227         endif;
371800130227
371900130227         // -?Override ai file di stampa?
372000130227         //  ?(per impostarvi i dati per l'invio via e-mail)?
372100130227         exsr sr_OvrPrtF;
372200130227
372300130227         // -?Stampa segnalazione?
372400130227         except  EmlTxt;
372500130227         P_Fgs2 = ds_Distinta.p_Fgs;
372600130227         P_Ndc2 = ds_Distinta.p_Ndc;
372700130227         except  EmlDet;
372800130227         except  EmlEnd;
372900130227
373000130227         // -?Cancellazione Override ai file di stampa?
373100130227         exsr sr_DeleteOvr;
373200130227
373300130227       ENDSR;
373400130227
373500130227       //--------------------------------------------------------------
373600130716       //?Override al file di stampa per impostarvi i dati per         ?
373700130716       //?  l'invio via e-mail.                                        ?
373800130227       //--------------------------------------------------------------
373900130227       BEGSR  sr_OvrPrtF;
374000130227
374100130227         reset  TRTCM1ds;
374200130227         if  §MRAdmitt <> *blank;
374300130227           §CM1mitt = %trim(§MRAdmitt);
374400130227         endif;
374500130227         §CM1dst  = %trim(§MRAddest);
374600130227         §CM1tips = §MRAdreg;
374700130227         //§CM1var  = '*OBJM*' + §MRAddes;
374800130227         §CM1idp  = §MRAdidpro;
374900130227
375000130227         // -?Override al "corpo e-mail"?
375100130227         if  §MRAdreg <> *blank;
375200130227           Qcmd = c_CmdOvrPrtF
375300130227                + ' outq(' + %trim(§MRAdoutqi) + ')'
375400130227                + ' usrdfndta(''' + TRTCM1ds + ''')';
375500130227         else;
375600130227           Qcmd = c_CmdOvrPrtF;
375700130227         endif;
375800130227
375900130227         exsr  sr_ExecCmd;
376000130227
376100130227         //if  Qusei <> *blank;
376200130227         //  ...;
376300130227         //endif;
376400130227
376500130227         // -?Apertura file di stampa?
376600130227         open  PrtEmail;
376700130227
376800130227       ENDSR;
376900130227
377000130227       //--------------------------------------------------------------
377100130227       //?Chiusura file di stampa e cancellazione override.            ?
377200130227       //--------------------------------------------------------------
377300130227       BEGSR  sr_DeleteOvr;
377400130227
377500130227         // -?Chiusura file di stampa?
377600130227         if  %open(PrtEMAIL);
377700130227           close  PrtEMAIL;
377800130227         endif;
377900130227
378000130227         // -?Cancellazione override al file di stampa?
378100130227         Qcmd = c_CmdDltOvr;
378200130227         exsr  sr_ExecCmd;
378300130227
378400130227       ENDSR;
378500130227
378600130227       //--------------------------------------------------------------
378700130227       //?Esecuzione del comando (già impostato).                      ?
378800130227       //--------------------------------------------------------------
378900130227       BEGSR  sr_ExecCmd;
379000130227
379100130227         clear Qcap0100;
379200130227         Qcabcsdh = *off;
379300130227         Qcapa    = *off;
379400130227         Qcacmdss = *off;
379500130227         Qcaerved = *allX'00';
379600130227
379700130227         clear Qusec;
379800130227         Qusbprv  = %size(Qusec);
379900130227
380000130227         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
380100130227                           %size(Qcap0100) : 'CPOP0100' : *omit :
380200130227                           0 : 0 : Qusec);
380300130227
380400130227         //if  Qusei <> *blank;
380500130227         //  ...;
380600130227         //endif;
380700130227
380800130227       ENDSR;
380900101221
381000080724      /end-free
381100030515      *
381200030515      *---------------------------------------------------------------*
381300050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
381400030515      *---------------------------------------------------------------*
381500030515     c     sr_DS_ARB     BEGSR
381600030515      *
381700030515     c                   eval      DB0cbo = ARBcbo
381800030515     c                   eval      DB0tsp = ARBtsp
381900030515     c                   eval      DB0mgs = ARBmgs
382000030515     c                   eval      DB0tc1 = ARBtc1
382100030515     c                   eval      DB0tc2 = ARBtc2
382200030515     c                   eval      DB0fns = ARBfns
382300030515     c                   eval      DB0rmn = ARBrmn
382400030515     c                   eval      DB0rma = ARBrma
382500030515     c                   eval      DB0ias = ARBias
382600030515     c                   eval      DB0vas = ARBvas
382700030515     c                   eval      DB0vmd = ARBvmd
382800030515     c                   eval      DB0vad = ARBvad
382900030515     c                   eval      DB0qft = ARBqft
383000030515     c                   eval      DB0lna = ARBlna
383100030515     c                   eval      DB0znc = ARBznc
383200030515     c                   eval      DB0rsm = ARBrsm
383300030515     c                   eval      DB0inm = ARBinm
383400030515     c                   eval      DB0cam = ARBcam
383500030515     c                   eval      DB0lom = ARBlom
383600030515     c                   eval      DB0prm = ARBprm
383700030515     c                   eval      DB0nzm = ARBnzm
383800030515     c                   eval      DB0fap = ARBfap
383900030515     c                   eval      DB0rmo = ARBrmo
384000030515     c                   eval      DB0rsd = ARBrsd
384100030515     c                   eval      DB0ind = ARBind
384200030515     c                   eval      DB0cad = ARBcad
384300030515     c                   eval      DB0lod = ARBlod
384400030515     c                   eval      DB0prd = ARBprd
384500030515     c                   eval      DB0nzd = ARBnzd
384600030515     c                   eval      DB0fin = ARBfin
384700030515     c                   eval      DB0gc1 = ARBgc1
384800030515     c                   eval      DB0gc2 = ARBgc2
384900030515     c                   eval      DB0nas = ARBnas
385000030515     c                   eval      DB0ncl = ARBncl
385100030515     c                   eval      DB0pkf = ARBpkf
385200030515     c                   eval      DB0fvf = ARBfvf
385300030515     c                   eval      DB0vlf = ARBvlf
385400030515     c                   eval      DB0fls = ARBfls
385500030515     c                   eval      DB0ncd = ARBncd
385600030515     c                   eval      DB0nca = ARBnca
385700030515     c                   eval      DB0xco = ARBxco
385800030515     c                   eval      DB0ffd = ARBffd
385900030515     c                   eval      DB0dcr = ARBdcr
386000030515     c                   eval      DB0hcr = ARBhcr
386100030515     c                   eval      DB0tcr = ARBtcr
386200030515     c                   eval      DB0fbr = ARBfbr
386300030515     c                   eval      DB0gga = ARBgga
386400030515     c                   eval      DB0gma = ARBgma
386500030515     c                   eval      DB0gva = ARBgva
386600030515     c                   move      ' '           DB0gva
386700030515     c                   clear                   DB0sop
386800030515     c                   eval      DB0ctr = ARBctr
386900030515     c                   clear                   DB0drt
387000030515     c                   clear                   DB0fpp
387100030515     c                   clear                   DB0nrt
387200030515     c                   clear                   DB0pdr
387300030515      *
387400030515      * Allega documento di trasporto
387500060524      * & Codice cliente da Assegnato
387600130924     c***?(già fatto:)?  clear                   dsBL4A
387700031029     c                   clear                   DB0bam
387800061107     c                   exsr      RepAR4_A
387900060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
388000031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
388100030515     c                   movel     'S'           DB0bam
388200060505     c                   endif
388300030515      *
388400030515      * Destinatario: vedo se esiste 2ª ragione sociale
388500061107     c                   clear                   DB0rd2
388600061107     c                   exsr      RepAR4_D
388700030515      *
388800030515      * Note
388900061107     c                   clear                   DB0nt1
389000061107     c                   clear                   DB0nt2
389100061107     c                   exsr      RepAR4_89
389200030515      *
389300030515     c                   ENDSR
389400030515      *
389500030515      *---------------------------------------------------------------*
389600050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
389700050217      *?   "generiche" (non da "1A")                                 ?*
389800030515      *---------------------------------------------------------------*
389900030515     c     sr_DS_TAB     BEGSR
390000030515      *
390100030515      * Carico le tabelle "generiche" (3A, TB, 5E)
390200030515     c                   exsr      CarTAB
390300030515      *
390400030515      * Codici bolla
390500030515     c                   eval      DB0tb1 = §3Atb1
390600030515     c                   eval      DB0tb2 = §3Atb2
390700030515     c                   eval      DB0dcb = §3Ades
390800030515      * Tipo servizio
390900030515if  1c                   if        DB0dts = *blanks
391000030515     c                   eval      DB0dts = §5Edes
391100030515     c                   eval      DB0fst = §5Efst
391200030515e   1c                   endif
391300030515      * Consegne particolari
391400030515if  1c                   if            DB0tc1 <> *blanks
391500030515     c                             and DB0dc1 =  *blanks
391600030515     c                   movel(p)  DB0tc1        TBLkey
391700030515     c                   exsr      Tab_1P
391800030515     c                   eval      DB0dc1 = §1Pdes
391900030515e   1c                   endif
392000030515if  1c                   if            DB0tc2 <> *blanks
392100030515     c                             and DB0dc2 =  *blanks
392200030515     c                   movel(p)  DB0tc2        TBLkey
392300030515     c                   exsr      Tab_1P
392400030515     c                   eval      DB0dc2 = §1Pdes
392500030515e   1c                   endif
392600030515      * Esenzione IVA
392700030515if  1c                   if            DB0cei <> *blanks
392800030515     c                             and DB0dce =  *blanks
392900030515     c                   eval      DB0dce =  §EIdes
393000030515e   1c                   endif
393100030515      *
393200030515     c                   ENDSR
393300060524      *
393400060524      *---------------------------------------------------------------*
393500060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
393600061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
393700060524      *---------------------------------------------------------------*
393800060524     c     RepAR4_A      BEGSR
393900060524      *
394000130924     c***?(già fatto:)?  clear                   dsBL4A
394100061107      *
394200060524     c                   eval      Wtrc = 'A'
394300060524     c     KLDVtrc       chain     FIAR4000
394400060524      *
394500060524if  1c                   if        %found(FIAR401L)
394600060524     c                   movel     AR4Not        dsBL4A
394700060524e   1c                   endif
394800060524      *
394900060524     c                   ENDSR
395000061107      *
395100061107      *---------------------------------------------------------------*
395200061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
395300061107      *?(2ª ragione sociale del destinatario)                        ?*
395400061107      *---------------------------------------------------------------*
395500061107     c     RepAR4_D      BEGSR
395600061107      *
395700130924     c***?NON esiste:?   clear                   dsBL4D
395800130924     c***?(già fatto:)?  clear                   DB0rd2
395900061107      *
396000061107     c                   eval      Wtrc = 'D'
396100061107     c     KLDVtrc       chain     FIAR4000
396200061107      *
396300061107if  1c                   if        %found(FIAR401L)
396400061107     c                   movel     AR4not        DB0rd2
396500061107e   1c                   endif
396600061107      *
396700061107     c                   ENDSR
396800171117      *//*
396900171117      *//*---------------------------------------------------------------*
397000171117      *//*?Reperimento dati da FIAR400F x trk "I"                       ?*
397100171117      *//*?(dati DPD)                                                   ?*
397200171117      *//*---------------------------------------------------------------*
397300171117     c*//  RepAR4_I      BEGSR
397400061030      *
397500130924     c***?(già fatto:)?  clear                   dsBL4I
397600061107      *
397700171117     c*//                eval      Wtrc = 'I'
397800171117     c*//  KLDVtrc       chain     FIAR4000
397900061030      *
398000171117if  1c*//                if        %found(FIAR401L)
398100171117     c*//                movel     AR4not        dsBL4I
398200171117e   1c*//                endif
398300061030      *
398400171117     c*//                ENDSR
398500071008      *
398600071015      *---------------------------------------------------------------*
398700071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
398800110204      *?(dati aggiuntivi per O.R.M. abbinato alla spedizione)        ?*
398900071015      *---------------------------------------------------------------*
399000071015     c     RepAR4_M      BEGSR
399100071015      *
399200130924     c***?(già fatto:)?  clear                   dsBL4M
399300071015      *
399400071015     c                   eval      Wtrc = 'M'
399500071015     c     KLDVtrc       chain     FIAR4000
399600071015      *
399700071015if  1c                   if        %found(FIAR401L)
399800071015     c                   movel     AR4Not        dsBL4M
399900071015e   1c                   endif
400000071015      *
400100071015     c                   ENDSR
400200121018      *
400300121018      *---------------------------------------------------------------*
400400121018      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
400500121018      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
400600121018      *---------------------------------------------------------------*
400700121018     c     RepAR4_Q      BEGSR
400800121018      *
400900130924     c***?(già fatto:)?  clear                   dTA4Q
401000130924     c***?(già fatto:)?  clear                   DB0cpi
401100121018      *
401200121018     c                   eval      Wtrc = 'Q'
401300121018     c     KLDVtrc       chain     FIAR4000
401400121018      *
401500121018if  1c                   if        %found(FIAR401L)
401600121018     c                   movel     AR4not        dTA4Q
401700121018e   1c                   endif
401800121018      *
401900121018     c                   ENDSR
402000080523      /free
402100080523
402200080523       //--------------------------------------------------------------
402300080523       //?Reperimento dati da FIAR400F x trk "1"                       ?
402400080523       //?(nominativo del destinatario)                                ?
402500080523       //--------------------------------------------------------------
402600080523       BEGSR  RepAR4_1;
402700080523
402800101222         //clear  dsBL41;        *** NON esiste ***?
402900080523
403000080523         Wtrc = '1';
403100080523         chain  KLDVtrc  FIAR4000;
403200080523
403300080523         if  NOT  %found(FIAR401L);
403400080523           clear  AR4not;
403500080523         endif;
403600080523
403700150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
403800081104           Pfedkcn = AR4not;
403900081104         else;
404000081104           Pkcn    = AR4not;
404100081104         endif;
404200080523
404300080523       ENDSR;
404400080523
404500080523      /end-free
404600061107      *
404700061107      *---------------------------------------------------------------*
404800061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
404900061107      *?(note)                                                       ?*
405000061107      *---------------------------------------------------------------*
405100061107     c     RepAR4_89     BEGSR
405200061107      *
405300130924     c***?NON esiste:?   clear                   dsBL48
405400130924     c***?NON usata:?    clear                   dsBL49
405500130924     c***?(già fatto:)?  clear                   DB0nt1
405600130924     c***?(già fatto:)?  clear                   DB0nt2
405700061107      *
405800061107     c                   eval      Wtrc = '8'
405900061107     c     KLDVtrc       chain     FIAR4000
406000061107if  1c                   if        %found(FIAR401L)
406100061107     c                   movel     AR4not        DB0nt1
406200061107e   1c                   endif
406300061107      *
406400061107     c                   eval      Wtrc = '9'
406500061107     c     KLDVtrc       chain     FIAR4000
406600061107if  1c                   if        %found(FIAR401L)
406700061107     c                   movel     AR4not        DB0nt2
406800061107e   1c                   endif
406900061107      *
407000061107     c                   ENDSR
407100171117      *
407200171117      *---------------------------------------------------------------*
407300171117      *?Reperimento dati da FIPND00F                                 ?*
407400171117      *?(dati DPD)                                                   ?*
407500171117      *---------------------------------------------------------------*
407600171117     c     RepFIPND      BEGSR
407700171117      *
407800171117if  1c                   if        NOT  %open(FIPND01L)
407900171117     c                   open      FIPND01L
408000171117e   1c                   endif
408100171117      *
408200171117     c     kLDV          chain     FIPND000
408300171117      *
408400171117if  1c                   if        NOT %found(FIPND01L)
408500171117     c                   clear                   PNDipn
408600171117e   1c                   endif
408700171117      *
408800171117     c                   ENDSR
408900030515      *
409000030515      *---------------------------------------------------------------*
409100050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
409200030515      *---------------------------------------------------------------*
409300030515     c     CarTAB        BEGSR
409400030515      *
409500030515      * Tab. "3A" - Codici Bolla
409600030515      * (NO se rimesso mittente)
409700030515     c                   clear                   ds3A
409800030515if  1c                   if        DB0ris <> 'M'
409900030515     c                   movel     '3A'          TBLcod
410000030515     c                   movel(p)  DB0cbo        TBLkey
410100030515     c     K03TAB        chain     TABEL
410200030515if  2c                   if        %found(TABEL00F)
410300030515     c                   movel     TBLuni        ds3A
410400030515e   2c                   endif
410500030515e   1c                   endif
410600070515     c                   eval      $Mamma =  (§3Abcm = 'S')
410700030515      *
410800030515      * Tab. "TB" - Tipi Bolla
410900030515      * (NO se rimesso mittente)
411000030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
411100030515      * se no non stampo i dati fattura
411200030515     c                   clear                   dsTB
411300030515if  1c                   if        DB0ris <> 'M'
411400030515     c                   movel     'TB'          TBLcod
411500121029if  2c                   if        §3Atb2 <> *blanks
411600030515     c                   movel(p)  §3Atb2        TBLkey
411700030515x   2c                   else
411800030515     c                   movel(p)  §3Atb1        TBLkey
411900030515e   2c                   endif
412000030515     c     K03TAB        chain     TABEL
412100030515if  2c                   if        %found(TABEL00F)
412200030515     c                   movel     TBLuni        dsTB
412300030515e   2c                   endif
412400030515e   1c                   endif
412500030515      *
412600030515      * Tab. "5E" - Tipi Servizio
412700030515     c                   clear                   ds5E
412800090514if  1c                   if        DB0tsp <> *blanks
412900030515     c                   movel     '5E'          TBLcod
413000030515     c                   movel(p)  DB0tsp        TBLkey
413100030515     c     K03TAB        chain     TABEL
413200030515if  2c                   if        %found(TABEL00F)
413300030515     c                   movel     TBLuni        ds5E
413400030515e   2c                   endif
413500030515e   1c                   endif
413600030515      *
413700030911      * Tab. "EI" - Esenzioni I.V.A.
413800030515     c                   clear                   dsEI
413900030911      * Tab. "1A" - Tipi incasso contrassegno
414000030911     c                   clear                   ds1A
414100030911      * Tab. "1P" - Consegne particolari
414200030911     c                   clear                   ds1P
414300030515      *
414400030515     c                   ENDSR
414500030911      *
414600030911      *---------------------------------------------------------------*
414700050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
414800030911      *---------------------------------------------------------------*
414900030911     c     Tab_EI        BEGSR
415000030911      *
415100030911     c                   clear                   dsEI
415200030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
415300030911     c                   movel     'EI'          TBLcod
415400030911     c                   movel(p)  DB0cei        TBLkey
415500030911     c     K03TAB        chain     TABEL
415600030911if  2c                   if        %found(TABEL00F)
415700030911     c                   movel     TBLuni        dsEI
415800030911e   2c                   endif
415900030911e   1c                   endif
416000030911     c                   eval      DB0dce =  §EIdes
416100030911      *
416200030911     c                   ENDSR
416300030515      *
416400030515      *---------------------------------------------------------------*
416500050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
416600030515      *---------------------------------------------------------------*
416700030515     c     Tab_1A        BEGSR
416800030515      *
416900030515     c                   clear                   ds1A
417000030515if  1c                   if        DB0cas <> *zeros
417100030515     c                   movel     '1A'          TBLcod
417200030515     c                   movel(p)  DB0tic        TBLkey
417300030515     c     K03TAB        chain     TABEL
417400030515if  2c                   if        %found(TABEL00F)
417500030515     c                   movel     TBLuni        ds1A
417600030515e   2c                   endif
417700030515e   1c                   endif
417800030515      *
417900030515      * Imposto i dati nella DS
418000030515     c                   eval      DB0dti = §1Ades
418100030515     c                   eval      DB0ucm = §1Aucm
418200030515     c                   eval      DB0ste = §1Aste
418300030515      *
418400030515     c                   ENDSR
418500030515      *
418600030515      *---------------------------------------------------------------*
418700050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
418800030515      *---------------------------------------------------------------*
418900030515     c     Tab_1P        BEGSR
419000030515      *
419100030515     c                   clear                   ds1P
419200030515     c                   movel     '1P'          TBLcod
419300130924     c***?già impostato:?movel(p)  DB0tc1/2      TBLkey
419400030515     c     K03TAB        chain     TABEL
419500030515if  1c                   if        %found(TABEL00F)
419600030515     c                   movel     TBLuni        ds1P
419700030515e   1c                   endif
419800030515      *
419900030515     c                   ENDSR
420000030515      *
420100030515      *---------------------------------------------------------------*
420200050217      *?Calcolo importi LDV                                          ?*
420300030515      *---------------------------------------------------------------*
420400061031     c     sr_DS_IMPO    BEGSR
420500030515      *
420600030515      * Vedo se esiste tassazione
420700030515      *  solo se tipo bolla contabilizzabile
420800061030if  1c                   IF        §TBfcb =  '1'
420900101213      *
421000061030     c                   eval      Wtrc   =  '1'
421100040607     c     KLDVtrc       chain     FIAR6000
421200101213      *
421300030515if  2c                   if        %found(FIAR601L)
421400030515     c                   movel     AR6div        DB0div
421500030515     c                   exsr      CtrDIV
421600030515      *
421700030515      * NON c'è 2ª bolla
421800101213if  3c                   if        DB0tb2 = *blanks
421900030515      *  Estremi fattura
422000030515     c                   z-add     AR6nft        DB0nft
422100030515     c                   z-add     AR6dft        DB0dft
422200130205     c                   z-add     AR6fiv        wB0fiv
422300101213      *  Se è assegnato => stampo tutta la tassaz.
422400101213if  4c                   if        %subst(DB0tb1:1:1) = 'A'
422500061030     c                   eval      Wtrc   =  '1'
422600030515     c                   exsr      RepTASSAZ
422700030515e   4c                   endif
422800030515e   3c                   endif
422900030515e   2c                   endif
423000030515      *
423100061030      * C'è 2ª bolla
423200101213if  2c                   IF        DB0tb2 <> *blanks
423300030515      *
423400061030     c                   eval      Wtrc   =  '2'
423500040607     c     KLDVtrc       chain     FIAR6000
423600030515      *
423700061030if  3c                   If        %found(FIAR601L)
423800030515     c                   movel     AR6div        DB0div
423900030515     c                   exsr      CtrDIV
424000030515     c                   movel     AR6cei        DB0cei
424100030515      *   Estremi fattura
424200030515     c                   z-add     AR6nft        DB0nft
424300030515     c                   z-add     AR6dft        DB0dft
424400130205     c                   z-add     AR6fiv        wB0fiv
424500030515if  4c                   if        AR6ksc <> *zeros
424600030515     c                   eval      DB0ccd =  AR6ksc
424700030515e   4c                   endif
424800061030     c                   eval      Wtrc   =  '2'
424900030515     c                   exsr      RepTASSAZ
425000061030e   3c                   EndIf
425100030515      *
425200030515e   2c                   ENDIF
425300030515      *
425400030515e   1c                   ENDIF
425500030515      *
425600030515      * Vedo se ha contrassegno
425700060505     c     KLDV          chain     FIAR9000
425800060505if  1c                   IF        %found(FIAR901L)
425900061030     c                   eval      DB0cas =  AR9cas
426000061030     c                   eval      DB0vca =  AR9vca
426100061030     c                   eval      DB0gca =  AR9gca
426200061030     c                   eval      DB0tic =  AR9tic
426300030515x   1c                   ELSE
426400030515     c                   clear                   DB0cas
426500030515     c                   clear                   DB0vca
426600030515     c                   clear                   DB0gca
426700030515     c                   clear                   DB0tic
426800030515     c                   clear                   DB0dti
426900030515     c                   clear                   DB0ucm
427000030515     c                   clear                   DB0ste
427100030515e   1c                   ENDIF
427200121018      *
427300131015      * Se P.A. con fattura immediata riporterò il Codice Fiscale
427400121018      *   (NON se destinatario estero)
427500121026if  1c                   if           ( %subst(DB0tb1:1:1) = 'A'
427600121018     c                             and (DB0nft  <> *zeros
427700121018     c                              or  DB0ift  <> *zeros)
427800121018     c                             and  DB0nzd  =  *blanks )
427900121018     c                   exsr      RepAR4_Q
428000121018     c                   movel     §TA4Qcfd      DB0cpi
428100121018e   1c                   endif
428200121018     c                   eval      wB0cfi = DB0cpi
428300061030      *
428400030515      * Vedo se si tratta di bolla figlia:
428500040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
428600070515if  1c                   if        $Mamma =  *on
428700030911     c                   exsr      BollaFiglia
428800030515e   1c                   endif
428900030515      *
429000030515     c                   ENDSR
429100030515      *
429200030515      *---------------------------------------------------------------*
429300050217      *?Decodifica LNA                                               ?*
429400030515      *---------------------------------------------------------------*
429500131203     c     sr_DecLNA     BEGSR
429600030515      *
429700030515if  1c                   if            DB0dla =  *blanks
429800030515     c                             and DB0ris <> 'M'
429900030515      *
430000040311     c                   eval      SAVlna = DB0lna
430100070312     c                   clear                   OG146arr
430200061016     c                   clear                   OG148arr
430300070312     c                   clear                   OG150arr
430400030515     c     DB0lna        chain     AZORG
430500030515      *
430600030515if  2c                   if        %found(AZORG01L)
430700030515     c                   eval      DB0fag = ORGfag
430800030515     c                   eval      DB0dla = ORGdes
430900030515     c                   eval      DB0inl = ORGind
431000030515     c                   eval      DB0cal = ORGcpf
431100030515     c                   eval      DB0lol = ORGloc
431200030515     c                   eval      DB0prl = ORGpro
431300030515     c                   eval      DB0tel = ORGtel
431400030515     c                   eval      DB0fax = ORGfax
431500070312     c                   movel     ORGde6        OG146arr
431600061016     c                   movel     ORGde8        OG148arr
431700070312     c                   movel     ORGdf0        OG150arr
431800030515e   2c                   endif
431900030515      *
432000030515e   1c                   endif
432100030515      *
432200030515     c                   ENDSR
432300030515      *
432400030515      *---------------------------------------------------------------*
432500050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
432600030515      *---------------------------------------------------------------*
432700030515     c     RepTASSAZ     BEGSR
432800030515      *
432900030515     c                   eval      DB0imv = AR6imv
433000030515     c                   eval      DB0cei = AR6cei
433100030515     c                   eval      DB0ift = AR6ift
433200101213      *
433300030911      * Esenzione IVA
433400030911if  1c                   if            DB0cei <> *blanks
433500030911     c                             and DB0dce =  *blanks
433600030911     c                   exsr      Tab_EI
433700030911e   1c                   endif
433800101213      *
433900101213      * Carico tabella VARIABILI ARRIVI
434000101213     c                   clear                   ds$2
434100101213     c                   eval      TBLcod = '$2'
434200101213     c                   eval      TBLkey = '1'
434300101213     c     K03TAB        chain     TABEL
434400101213if  1c                   if        %found(TABEL00F)
434500101213     c                   movel     TBLuni        ds$2
434600101213e   1c                   endif
434700030515      *
434800030515      * Controllo se ci sono varie esenti
434900030515      *
435000030515      * - varia 1
435100030515     c                   eval      Wsvx = AR6sv1
435200030515     c                   eval      Wvax = AR6va1
435300030515     c                   exsr      RepTASSAZ_b
435400030515      * - varia 2
435500030515     c                   eval      Wsvx = AR6sv2
435600030515     c                   eval      Wvax = AR6va2
435700030515     c                   exsr      RepTASSAZ_b
435800030515      * - varia 3
435900030515     c                   eval      Wsvx = AR6sv3
436000030515     c                   eval      Wvax = AR6va3
436100030515     c                   exsr      RepTASSAZ_b
436200030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
436300040607     c     KLDVtrc       setll     FIAR7000
436400040607     c     KLDVtrc       reade     FIAR7000
436500030515      *
436600030515do  2c                   Dow       Not %eof(FIAR701L)
436700030515      *
436800030515     c                   eval      Wsvx = AR7svn
436900030515     c                   eval      Wvax = AR7van
437000030515     c                   exsr      RepTASSAZ_b
437100030515      *
437200040607     c     KLDVtrc       reade     FIAR7000
437300030515e   2c                   Enddo
437400030515      *
437500030515     c                   ENDSR
437600030515      *
437700030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
437800050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
437900050217      *?- 2ª parte -                                                 ?*
438000030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
438100030515     c     RepTASSAZ_b   BEGSR
438200030515      *
438300030515sel 1c                   Select
438400030515      *
438500030515w   1c                   When      Wsvx       = *blanks
438600030515      *
438700030515w   1c                   When      Wsvx       = §$2iva
438800030515     c                   eval      DB0iva     = Wvax
438900030515      *
439000030515w   1c                   When      Wsvx       = §$2bol
439100030515     c                   add       Wvax          DB0ese
439200110117     c*//                z-add     Wvax          wBolloVirt
439300110117     c*//                eval      $BolloVirt = *on
439400030515      *
439500030515x   1c                   Other
439600101213      * Reperimento varie esenti IVA
439700101213     c                   movel     'CC'          TBLcod
439800101213     c                   movel(p)  Wsvx          TBLkey
439900101213     c     K03TAB        chain     TABEL
440000101213if  1c                   IF             %found(TABEL00F)
440100101213     c                             and  TBLflg = *blanks
440200101213     c                   movel     TBLuni        dsCC
440300101213if  2c                   if        §CCcei <> *blanks
440400101213     c                   add       Wvax          DB0ese
440500101213     c                   sub       Wvax          DB0imv
440600101213e   2c                   endif
440700101213e   1c                   ENDIF
440800030515      *
440900101213e   1c                   Endsl
441000030515      *
441100030515     c                   ENDSR
441200030515      *
441300030515      *---------------------------------------------------------------*
441400050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
441500030515      *---------------------------------------------------------------*
441600030515     c     TABxPAR       BEGSR
441700030515      *
441800100517     c                   movel(p)  WCdPar        w053a
441900030515     c                   movel     '7V'          TBLcod
442000030515     c                   movel(p)  KeyPAR        TBLkey
442100030515     c     K03TAB        chain     TABEL
442200030515      *
442300030515if  1c                   if        %found(TABEL00F)
442400030515     c                             and TBLflg = *blanks
442500100517     c                   eval      w053a = WCdPar + '-'
442600121108     c                                   + %trim(%subst(TBLuni:1:49))
442700030515x   1c                   else
442800030515     c                   clear                   TBLuni
442900030515e   1c                   endif
443000030515      *
443100030515     c                   ENDSR
443200030515      *
443300030515      *---------------------------------------------------------------*
443400050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
443500030515      *---------------------------------------------------------------*
443600030515     c     CtrCAs        BEGSR
443700030515      *
443800030515      * E' da convertire
443900030515if  1c                   if        DB0ucm = 'S'
444000030515     c                   eval      $CAeur = *on
444100030515e   1c                   endif
444200030515      *
444300030515      * Se in EURO è da convertire e da stampare in EURO
444400030515if  1c                   if        DB0vca = §GEDcn
444500030515     c                   eval      $CAeur = *on
444600030515     c                   endif
444700030515      *
444800030515      * Devo convertire il c/a da valuta a moneta di conto
444900030624if  1c                   if        DB0vca <> §GEDcn
445000030515     c                   clear                   YEURCODS
445100030515     c                   movel     DB0vca        YECdvi
445200030515     c                   z-add     DB0cas        YECimi
445300030515     c                   movel     §GEDcn        YECdvo
445400030515     c                   call      'YEURCO'
445500030515     c                   parm                    YEURCODS
445600030515if  2c                   if        YECesi = *blanks
445700030515     c                   z-add     YECimo        Wcas
445800030515e   2c                   endif
445900030515x   1c                   else
446000030515     c                   z-add     DB0cas        Wcas
446100030515e   1c                   endif
446200030515      *
446300030515     c                   ENDSR
446400030515      *
446500030515      *---------------------------------------------------------------*
446600050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
446700030515      *---------------------------------------------------------------*
446800030515     c     CtrDIV        BEGSR
446900030515      *
447000030515     c                   clear                   dsCV
447100030515      *
447200030515     c                   movel     'CV'          TBLcod
447300030515     c                   movel(p)  DB0div        TBLkey
447400030515     c     K03TAB        chain     TABEL
447500030515     c                   if        %found(TABEL00F)
447600030515     c                   movel     TBLuni        dsCV
447700030515     c                   movel     §CVfdc        DB0fdc
447800030515     c                   endif
447900030515      *
448000030515     c                   ENDSR
448100030515      *
448200030515      *---------------------------------------------------------------*
448300050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
448400030515      *---------------------------------------------------------------*
448500030911     c     BollaFiglia   BEGSR
448600030515      *
448700090619     c                   if        not %open(FNLBL01L)
448800101213     c                   open(e)   FNLBL01L
448900101213      /free
449000101213           if  %error;
449100101213             exsr  sr_ErrOpenFile;
449200101213           endif;
449300101213      /end-free
449400090619     c                   endif
449500090619      *
449600040607     c     KLDV          chain     FNLBL000
449700030515if  1c                   if        %found(FNLBL01L)
449800030515      *
449900030515     c     KLBL          chain     FNARB000
450000030515if  2c                   if        %found(FNARB01L)
450100030515      *
450200031015     c                   eval      wB0nrs = ARBnrs
450300030515     c                   eval      DB0fns = ARBfns
450400030515     c                   eval      DB0fls = ARBfls
450500030515     c                   eval      DB0ncd = ARBncd
450600030515     c                   eval      DB0nca = ARBnca
450700050114     c**!!!              eval      DB0xco = ARBxco
450800030515     c                   eval      DB0znc = ARBznc
450900030515      * Aggancio AR9
451000060505     c     KLBL          chain     FIAR9000
451100060505if  3c                   IF        %found(FIAR901L)
451200030515     c                   eval      DB0cas = AR9cas
451300030515     c                   eval      DB0vca = AR9vca
451400030515     c                   eval      DB0gca = AR9gca
451500030515     c                   eval      DB0tic = AR9tic
451600030515x   3c                   ELSE
451700030515     c                   clear                   DB0cas
451800030515     c                   clear                   DB0vca
451900030515     c                   clear                   DB0gca
452000030515     c                   clear                   DB0tic
452100030515     c                   clear                   DB0dti
452200030515     c                   clear                   DB0ucm
452300030515     c                   clear                   DB0ste
452400030515e   3c                   ENDIF
452500030515      *
452600030515e   2c                   endif
452700030515      *
452800030515e   1c                   endif
452900030515      *
453000030515     c                   ENDSR
453100030903      *
453200030515      *---------------------------------------------------------------*
453300050217      *?DEFINIZIONE KEY-LIST                                         ?*
453400030515      *---------------------------------------------------------------*
453500030515     c     DefKLIST      BEGSR
453600030515      *
453700090619      * - FNARB01L / FNART01L /
453800030515      *   FNLBL01L /
453900060505      *   FIAR501L/31C / FIAR901L
454000040607     c     KLDV          klist
454100030515     c                   kfld                    DB0aas
454200030515     c                   kfld                    DB0lnp
454300030515     c                   kfld                    DB0nrs
454400030515     c                   kfld                    DB0nsp
454500060505      * - FIAR401L / FIAR601L / FIAR701L
454600060505     c     KLDVtrc       klist
454700060505     c                   kfld                    DB0aas
454800060505     c                   kfld                    DB0lnp
454900060505     c                   kfld                    DB0nrs
455000060505     c                   kfld                    DB0nsp
455100060505     c                   kfld                    Wtrc
455200040607      * - FIAR501L/31C
455300030717     c     K05AR501      klist
455400030717     c                   kfld                    DB0aas
455500030717     c                   kfld                    DB0lnp
455600030717     c                   kfld                    DB0nrs
455700030717     c                   kfld                    DB0nsp
455800030717     c                   kfld                    AR5trd
455900090619      * - FNARB01L / FIAR901L di bolla MAMMA
456000070514     c     KLBL          klist
456100070514     c                   kfld                    LBLaap
456200070514     c                   kfld                    LBLlpp
456300070514     c                   kfld                    LBLnrp
456400070514     c                   kfld                    LBLnsp
456500081030      * - FNART01L di bolla MAMMA
456600081030     c     KLBLnsc       Klist
456700081030     c                   kfld                    LBLaap
456800081030     c                   kfld                    LBLlpp
456900081030     c                   kfld                    LBLnrp
457000081030     c                   kfld                    LBLnsp
457100081030     c                   Kfld                    Wnsc
457200081030     c     K05ARS01A     klist
457300081030     c                   kfld                    ARTfls
457400081030     c                   kfld                    ARTlna
457500081030     c                   kfld                    ARTnrs
457600081030     c                   kfld                    ARTnsc
457700081030     c                   kfld                    ARStrc
457800081030      * - FNART01L
457900081030     c     KLDVnsc       Klist
458000081030     c                   Kfld                    DB0aas
458100081030     c                   Kfld                    DB0lnp
458200081030     c                   Kfld                    DB0nrs
458300081030     c                   Kfld                    DB0nsp
458400081030     c                   Kfld                    Wnsc
458500100517      * - FNORM01L
458600100517     c     K04ORM01      klist
458700100517     c                   kfld                    ORMpoe
458800100517     c                   kfld                    ORMnsr
458900100517     c                   kfld                    ORMnor
459000100517     c                   kfld                    ORMnrv
459100030515      * - TABEL00F
459200030515     c     K03TAB        klist
459300040223     c                   kfld                    TBLkut
459400030515     c                   kfld                    TBLcod
459500030515     c                   kfld                    TBLkey
459600030515      *
459700030515     c                   ENDSR
459800080430
459900080430       //--------------------------------------------------------------
460000140303       //?Operazioni finali                                            ?
460100080430       //--------------------------------------------------------------
460200080430     c     sr_End        BEGSR
460300080430      *
460400080430     c                   clear                   TIBS02ds
460500080430     c                   eval      T02tla = 'C'
460600080430     c                   call      'TIBS02R'
460700080430     c                   parm                    KPJBA
460800080430     c                   parm                    TIBS02ds
460900080430     c                   reset                   FNLV80ds
461000080430     c                   call      'FNLV80R'
461100080430     c                   parm                    KPJBA
461200080430     c                   parm                    FNLV80ds
461300080430      /free
461400101222         if  §VPOrcm <= wDate and
461500101222             DB0tsp   = 'E'   and
461600101222            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
461700101222                                   DB0lnp <> 006   and
461800101222                                   DB0lnp <> 063));
461900101222           reset  TNSD99ds;
462000101222           tnsd99r ( TNSD99ds );
462100110117         endif;
462200080724
462300101222         // -?Chiusura del work-file?- - - - - - - - - - - - - - - - -*
462400090210
462500110202         if  %open(FIILV01L);
462600110202           close  FIILV01L;
462700090210         endif;
462800080430
462900101222         // -?Restituzione parametri?- - - - - - - - - - - - - - - - -*
463000080508
463100110221         if wCod_Log  <> '01';
463200130206           RootFolder = §ILVimgSca;
463300110221         endif;
463400110221
463500080520         if Risultato <> 'OK';
463600080718
463700080731           clear FileName;
463800080731           clear TemplateName;
463900080508
464000130213           for wx = 1 to c_Row;
464100130206             %occur(LdV) = wx;
464200130206             LdvPrg      = wx;
464300130206           endfor;
464400080508
464500080508         endif;
464600080430
464700131203
464800080430           exec sql   set result sets
464900080430                      for return to client
465000081103                      array :LdV   for :w_row rows;
465100080430
465200080430           select;
465300080430             when sqlCode = 100;
465400080430               RtnStatus  = sqlCode;
465500130924               //RtnOpCode  = 'SQLCODE   ';
465600130924               //RtnEsito   = -1;
465700080430             when sqlCode < *zero;
465800080430               RtnStatus  = sqlCode;
465900130924               //RtnOpCode  = 'SQLCODE   ';
466000130924               //RtnEsito   = -1;
466100080430           endsl;
466200080430
466300080430
466400080430         return;
466500080430
466600080430      /end-free
466700080430     c                   ENDSR
466800080430
466900080430      *---------------------------------------------------------------*
467000130227
467100140303       //--------------------------------------------------------------
467200140303       //?Spool di stampa (per e-mail).                                ?
467300140303       //--------------------------------------------------------------
467400130227
467500130227     oPrtEMAIL  e            EmlTxt            2
467600130227     o                       SDSpgm
467700130227     o                                        +   1 '-'
467800130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
467900130227     o                                              ABORATA 100 VOLTE'
468000130227     o          e            EmlTxt      0
468100130227     o                       SDSpgm
468200130227     o                                        +   1 '-'
468300130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
468400130227     o                                              ABORATA 100 VOLTE'
468500130227     o          e            EmlTxt      2
468600130227     o                                              'Spedizione     '
468700130227     o                                        +   3 'Distinta  '
468800130227     o          e            EmlTxt      1  1
468900130227     o                                              '------------------
469000130227     o                                              --'
469100130227     o                                        +   3 '----------'
469200130227     o          e            EmlDet      1
469300130227     o                       DB0aas        3
469400130227     o                       DB0lnp        3  +   1
469500130227     o                       DB0nrs        3  +   1
469600130227     o                       DB0nsp        3  +   1
469700130227     o                       P_Fgs2        3  +   3
469800130227     o                       P_Ndc2        3  +   1
469900130227     o          e            EmlEnd      2
470000130227     o                                              '***  Fine Lista  ***'
470100130227     o          e            EmlEnd      0
470200130227     o                                              '***  Fine Lista  ***'
470300080724
470400140303       //--------------------------------------------------------------
470500140303       //?Spool per immagine della LdV.                                ?
470600140303       //--------------------------------------------------------------
470700080430     oQSYSPRT   e            PRTtxt            3
470800080430     o                                              '   '
470900080430     o                       c_Char           +   0
471000080430     o          e            PRTrow      1
471100080430     o                       LDV              +   0
471200110201     o          e            PRTend      3
471300110201     o                       LastRow          +   0
471400080430
471500080718       //--------------------------------------------------------------
471600140303       //?Procedure name: getCurrentSystemName.                        ?
471700140303       //?Purpose:        Reperisce l'attributo di rete current System Name?
471800140303       //?Returns:        Current System Name                          ?
471900080718       //--------------------------------------------------------------
472000080718     p getCurrentSystemName...
472100080718     p                 b
472200080718
472300080718     d getCurrentSystemName...
472400080718     d                 pi             8a
472500080718
472600080718      /free
472700080718
472800080718       clear qUSEC;
472900080718       qUSBPRV = %size(qUSEC);
473000080718
473100080718       RetrieveNetworkAttributes( QWCRNETA_dataReturned
473200080718                                : %size(qWCRNETA_dataReturned)
473300080718                                : 1
473400080718                                : 'SYSNAME   '
473500080718                                : qUSEC
473600080718                                );
473700080718
473800080718       if  qUSEI <> *blank   or
473900080718           qWCRNETA_dataReturned.qWCRDR.qWCNBRAR = *zero;
474000080718         dump(A);
474100080718         clear qWCRNETA_networkAttributeInformationTable;
474200080718       else;
474300080718         qWCRNETA_networkAttributeInformationTable =
474400080718                                       qWCRNETA_dataReturned.qWCARRAY;
474500080718       endif;
474600080718
474700080718       return  qWCRNETA_networkAttributeInformationTable.qWCDATA;
474800080718
474900080718      /end-free
475000080718
475100080718     p getCurrentSystemName...
475200080718     p                 e
475300080718
475400110202       //--------------------------------------------------------------
475500110202       //?Schiere a tempo di compilazione.                             ?
475600110202       //--------------------------------------------------------------
475700030515
475800030515** > $GgS/$Gio * --------------------------------------------------------------*
4759000306241LUNEDI
4760000306242MARTEDI
4761000410263MERCOLEDI
4762000306244GIOVEDI
4763000306245VENERDI
4764000305156SABATO
4765000305157DOMENICA
476600030515M          MATT.
476700030515P          POM.
476800110202** -?$CoL?(Codice LOG)?/?$DeL?(Decodifica codice LOG)?
47690011020200Spedizione in distinta chiusa - da elaborare
47700011020201Spedizione elaborata
47710011032808Sped. non consegnata in dist. - immagine a SCARTO
47720011032509LdV da scannerizzare - immagine a SCARTO
47730011020321Parametri di input errati - immagine NON creata
47740011020822Distinta non trovata/annullata - immagine a SCARTO
47750011020823Sped./Dist. non trovata in FIILV - immag. a SCARTO
47760011020325Sped. non trovata in FNARB - immagine NON creata
47770013031351BOLLA RECUPERO - Immagine già generata
47780013031352BOLLA RECUPERO - Spedizione non trovata in FNARB
