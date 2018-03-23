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
008900110117
009000131017       // -?Dati per LdV partenza (solo per bolla mamma - cliente CLN)?
009100110117     fFNBLP01L  if   e           k disk    usropn
009200101213
009300101207       // -?O.R.M.?
009400100517     fFNORM01L  if   e           k disk    usropn
009500101213
009600110309       // -?File LOG spedizioni elaborate per immagine LdV?
009700110202     fFIILV01L  Uf A e           k disk    usropn
009800130227
009900130227       // -?Spool da inviare via e-mail:?
010000130227       //   ·?Corpo del messaggio di posta elettronica?
010100130227     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOE)
010200101213
010300101207       // -?Printer File  (se richiesta bozza in stampa)?
010400090210     fQSYSPRT   o    f  132        printer usropn  oflind(*inOF)
010500030515
010600101213       //--------------------------------------------------------------
010700101213       //?Definizione costanti.                                        ?
010800101213       //--------------------------------------------------------------
010900030515
011000110117     d c_DateMax_A     c                   const('20391231')
011100170517       // -?Capoconto clienti?
011200170517     d c_Kci           c                   const(0151)
011300131008       // -?Costanti in output?
011400110117     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
011500030515     d                                     NON SEQUENZIALI')
011600110117     d c_CoOr          c                   const('COLLI ORIGINALI     -
011700030717     d                                     N. ')
011800110117     d c_PMxA          c                   const('PROMEMORIA PER L''AU-
011900070315     d                                     TOTRASPORTATORE')
012000101213       // -?Codice cliente "FedEx"?
012100150430     d c_CliFedEx      c                   const(0501331)
012200150430     d c_CliFedEx2     c                   const(0506514)
012300101213       // -?Codice cliente "PraOil oleodotti italiani"?
012400150430     d c_CliPraOil     c                   const(0201515)
012500101213       // -?Codice cliente "TTL"?
012600091116     d c_CliTTL        c                   const(0504450)
012700101213       // -?Codice cliente "Lotterie Nazionali - Gratta & Vinci"?
012800090619     d c_CliCLN        c                   const(0632910)
012900140513     d c_CliLN_2       c                   const(0017093)
013000101213       // -?Codice cliente "S G Soluzioni Logistiche"?
013100090619     d c_CliSG         c                   const(0634923)
013200091117     d c_CliSG_2       c                   const(0635042)
013300110923
013400110923       // -?Consegna CON Riserva specifica?
013500110923     d c_RiservaSpecif...
013600110923     d                 c                   const('RS')
013700091117
013800131008       // -?Costante per evitare l'indicazione del contratto x "S.G."?
013900091117     d c_NoContr       c                   const('NO CONTRATTO')
014000101213
014100101213       // -?Costante per controllo "caratteri solo numerici"?
014200110126     d c_Digits        c                   const('0123456789')
014300101207       // -?Numero di righe da gestire per la LdV?
014400130213     d c_Row           c                   const(36)
014500081027     d c_Row_B         c                   const(49)
014600130213     d c_Row_B2        c                   const(51)                            c_Row_B + 2
014700081030     d c_Row_Fed       c                   const(10)
014800101207       // -?Sequenza numerica?
014900080430     d c_Char          c                   const('*...+....1....+....2+
015000080430     d                                            ....+....3....+....4+
015100080430     d                                            ....+....5....+....6+
015200080430     d                                            ....+....7....+....8+
015300080430     d                                            ....+....9....+...10+
015400080430     d                                            ....+...11....+...12')
015500130227
015600130227       // -?Dati di default?
015700130227     d c_Mittente      c                   const('ced')
015800130227     d c_at            c                   const('@brt.it')
015900130227     d c_Sede          c                   const('046')
016000130227
016100130227       // -?Comandi di override al PrtF "corpo e-mail"?
016200130227     d c_CmdOvrPrtF    c                   const('OVRPRTF +
016300130227     d                                           file(PRTEMAIL) +
016400130227     d                                           pagesize(66 132) +
016500130227     d                                           lpi(6) cpi(10) +
016600130227     d                                           ovrscope(*actgrpdfn) +
016700130227     d                                           ')
016800130227     d c_CmdDltOvr     c                   const('DLTOVR +
016900130227     d                                            file(PRTEMAIL) +
017000130227     d                                            lvl(*actgrpdfn)')
017100101213
017200101213       //--------------------------------------------------------------
017300101213       //?Definizione schiere.                                         ?
017400101213       //--------------------------------------------------------------
017500101213
017600101213       // -?Giorni della settimana e relativa decodifica?
017700070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
017800070306     d $Gio            s             15    dim(9)   alt($GgS)
017900110202
018000110202       // -?Codice / Decodifica LOG x FIILV00F?
018100110202     d $CoL            s                   like(ILVcol)
018200130313     d                                     dim(10)  ctdata  perrcd(1)
018300110202     d $DeL            s                   like(ILVnot)
018400130313     d                                     dim(10)  alt($CoL)
018500101213
018600101207       // -?Dati da restituire?
018700130214     d LdV             ds           123    occurs(c_Row_B2) inz
018800080508     d   LdvPrg                       3s 0 inz
018900080430     d   LdvTxt                     120    inz
019000101213
019100101213       // -?Note e Particolarità?
019200070306     d $NP             s                   dim(6)   inz  like(Pnp1)
019300101213       // -?Consegne Precedenti?
019400070319     d $CP             s                   dim(20)  inz(*all'9')
019500070319     d                                                   like(Elem_CP)
019600070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
019700101213
019800101213       //--------------------------------------------------------------
019900101213       //?Definizione strutture dati.                                  ?
020000101213       //--------------------------------------------------------------
020100101213
020200080430     d KPJBA         e ds                  inz
020300101213
020400131008       // -?Dati LDV da restituire per l'immagine?
020500080506     d FNLSB5ds      e ds                  inz
020600080430
020700110202       // -?Records in output?
020800110202       // ·? 1ª riga?
020900081021     d ds_01         e ds                  inz  extname(FISP3001ds)
021000081021     d                                          prefix(P : 3)
021100110202       // ·? 2ª riga (vuota)?
021200110202       // ·? 3ª riga?
021300080430     d ds_03         e ds                  inz  extname(FISP3003ds)
021400080430     d                                          prefix(P : 3)
021500110202       // ·? 4ª riga?
021600080430     d ds_04         e ds                  inz  extname(FISP3004ds)
021700080430     d                                          prefix(P : 3)
021800110202       // ·? 5ª riga?
021900080430     d ds_05         e ds                  inz  extname(FISP3005ds)
022000080430     d                                          prefix(P : 3)
022100110202       // ·? 6ª riga (vuota)?
022200110202       // ·? 7ª riga?
022300080430     d ds_07         e ds                  inz  extname(FISP3007ds)
022400080430     d                                          prefix(P : 3)
022500110202       // ·? 8ª riga (vuota)?
022600110202       // ·? 9ª riga?
022700080430     d ds_09         e ds                  inz  extname(FISP3009ds)
022800080430     d                                          prefix(P : 3)
022900110202       // ·?10ª riga?
023000080430     d ds_10         e ds                  inz  extname(FISP3010ds)
023100080430     d                                          prefix(P : 3)
023200110202       // ·?11ª riga?
023300080430     d ds_11         e ds                  inz  extname(FISP3011ds)
023400080430     d                                          prefix(P : 3)
023500110202       // ·?12ª riga?
023600080430     d ds_12         e ds                  inz  extname(FISP3012ds)
023700080430     d                                          prefix(P : 3)
023800110202       // ·?13ª riga?
023900080430     d ds_13         e ds                  inz  extname(FISP3013ds)
024000080430     d                                          prefix(P : 3)
024100110202       // ·?14ª riga?
024200080430     d ds_14         e ds                  inz  extname(FISP3014ds)
024300080430     d                                          prefix(P : 3)
024400130205     d ds_140        e ds                  inz  extname(FISP30140s)
024500130205     d                                          prefix(P : 3)
024600110202       // ·?15ª riga?
024700080430     d ds_15         e ds                  inz  extname(FISP3015ds)
024800080430     d                                          prefix(P : 3)
024900110202       // ·?16ª riga?
025000080430     d ds_16         e ds                  inz  extname(FISP3016ds)
025100080430     d                                          prefix(P : 3)
025200110202       // ·?17ª riga?
025300080430     d ds_17         e ds                  inz  extname(FISP3017ds)
025400080430     d                                          prefix(P : 3)
025500110202       // ·?18ª riga?
025600080430     d ds_18         e ds                  inz  extname(FISP3018ds)
025700080430     d                                          prefix(P : 3)
025800130205     d ds_180        e ds                  inz  extname(FISP30180s)
025900130205     d                                          prefix(P : 3)
026000110202       // ·?19ª riga?
026100080430     d ds_19         e ds                  inz  extname(FISP3019ds)
026200080430     d                                          prefix(P : 3)
026300110202       // ·?20ª riga?
026400080430     d ds_20         e ds                  inz  extname(FISP3020ds)
026500080430     d                                          prefix(P : 3)
026600110202       // ·?21ª riga?
026700080430     d ds_21         e ds                  inz  extname(FISP3021ds)
026800080430     d                                          prefix(P : 3)
026900110202       // ·?22ª riga?
027000080430     d ds_22         e ds                  inz  extname(FISP3022ds)
027100080430     d                                          prefix(P : 3)
027200110202       // ·?23ª riga?
027300080430     d ds_23         e ds                  inz  extname(FISP3023ds)
027400080430     d                                          prefix(P : 3)
027500110202       // ·?24ª riga?
027600080430     d ds_24         e ds                  inz  extname(FISP3024ds)
027700080430     d                                          prefix(P : 3)
027800110202       // ·?25ª riga?
027900080430     d ds_25         e ds                  inz  extname(FISP3025ds)
028000080430     d                                          prefix(P : 3)
028100110202       // ·?26ª riga (vuota)?
028200110202       // ·?27ª riga?
028300080430     d ds_27         e ds                  inz  extname(FISP3027ds)
028400080430     d                                          prefix(P : 3)
028500110202       // ·?28ª riga?
028600080430     d ds_28         e ds                  inz  extname(FISP3028ds)
028700080430     d                                          prefix(P : 3)
028800110202       // ·?29ª riga?
028900081021     d ds_29         e ds                  inz  extname(FISP3029ds)
029000081021     d                                          prefix(P : 3)
029100110202       // ·?30ª riga?
029200081021     d ds_30         e ds                  inz  extname(FISP3030ds)
029300081021     d                                          prefix(P : 3)
029400110215       // ·?31ª riga?
029500110215     d ds_31         e ds                  inz  extname(FISP3031ds)
029600110215     d                                          prefix(P : 3)
029700110202       // ·?32ª riga?
029800081021     d ds_32         e ds                  inz  extname(FISP3032ds)
029900081021     d                                          prefix(P : 3)
030000110202       // ·?33ª riga?
030100081021     d ds_33         e ds                  inz  extname(FISP3033ds)
030200081021     d                                          prefix(P : 3)
030300110202       // ·?34ª riga?
030400081021     d ds_34         e ds                  inz  extname(FISP3034ds)
030500081021     d                                          prefix(P : 3)
030600110202       // ·?35ª riga (vuota)?
030700110202       // ·?36ª riga (vuota)?
030800110202       // ·?37ª riga e successive?
030900081023     d ds_37         e ds                  inz  extname(FISP3037ds)
031000081021     d                                          prefix(P : 3)
031100110202       // ·?Righe x FedEx?
031200081030     d ds_Fed        e ds                  inz  extname(FISP304Fds)
031300081030     d                                          prefix(P : 3)
031400130411
031500130411       // -?Flag operativi in TITAS30C?
031600130411     d dTASflo       e ds                  inz     qualified
031700120702
031800130214       // -?Dati aggiuntivi in FIILV00F?
031900120702     d dILVlibero    e ds                  inz     qualified
032000101220
032100101220       // -?Parametri per conversione importi in Euro?
032200061107     d YEURCOds      e ds                  inz
032300110202
032400101220       // -?Dati da AZORG00F?
032500070424     d OG143ccm      e ds                  inz     extname(OG143)
032600070424     d                                             prefix(§CM:3)
032700070424     d OG146arr      e ds                  inz     extname(OG146)
032800070424     d                                             prefix(A)
032900070424     d OG146cf£6     e ds                  inz     extname(OG146)
033000070424     d                                             prefix(£6:1)
033100070424     d OG148arr      e ds                  inz     extname(OG148)
033200070424     d                                             prefix(A)
033300070424     d OG148cf£6     e ds                  inz     extname(OG148)
033400070424     d                                             prefix(£6:1)
033500070424     d OG150arr      e ds                  inz     extname(OG150)
033600070424     d                                             prefix(A)
033700110202
033800101220       // -?Dati da FIAR400F?
033900061108     d dsBL4A        e ds                  inz
034000061108     d dsBL4I        e ds                  inz
034100071015     d dsBL4M        e ds                  inz
034200121018     d dTA4Q         e ds                  inz
034300110228
034400110228       // -?Dati da FIDST00F?
034500110228     d dDSTflr       e ds                  inz
034600110202
034700101220       // -?Tabelle?
034800131206     d ds$2          e ds
034900061107     d ds1A          e ds                  inz
035000061107     d ds1P          e ds                  inz
035100070306     d ds2A          e ds                  inz
035200061107     d ds3A          e ds                  inz
035300061107     d ds5E          e ds                  inz
035400061107     d dsCC          e ds
035500061107     d dsCV          e ds                  inz
035600061107     d dsEI          e ds                  inz
035700061107     d dsTB          e ds                  inz
035800061107      *
035900030515     d TIBS02ds      e ds                  inz
036000040607     d   T02mod      e                     inz('C')
036100030805     d dGED          e ds
036200130206     d dILV          e ds
036300061031     d dVPOdeco      e ds                  inz
036400031124     d dAR5gen       e ds                  inz
036500061030     d dAR5bnb       e ds                  inz
036600170517     d dCLI          e ds                  inz
036700080724
036800131210       // -?Flag operativi di FNEVB - campo EVBNOT?
036900131206     d dEVB01        e ds                  inz     qualified
037000101220       // -?Flag operativi di FNORM - campo ORMFLO?
037100100517     d dORM01        e ds                  inz
037200101220       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
037300030515     d TRUL21ds      e ds                  inz
037400120702     d   I21tla      e                     inz('L')
037500101220       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
037600070212     d FNLV55ds      e ds                  inz
037700120702     d   D55tpt      e                     inz('6')
037800101220       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
037900051129     d FNLV80ds      e ds                  inz
038000120702     d   ILV80tla    e                     inz('C')
038100101220       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
038200090619     d TNSD99ds      e ds                  inz
038300120702     d   D98tla      e                     inz('C')
038400130227
038500130227       // -?Tab. "MRA" = Bart-Mailing?
038600130227     d dMRAdan       e ds                  inz
038700130227     d   §MRAddes    e                     inz('PDA-Oltre 100 log "01"+
038800130227     d                                             ')
038900130227     d   §MRAdreg    e                     inz('COR')
039000130227     d   §MRAdmitt   e                     inz(c_Mittente)
039100130227     d   §MRAddest   e                     inz('stefano.merighi+
039200130227     d                                          @brt.it;')
039300130227     d   §MRAdidpro  e                     inz('1')
039400130227     d   §MRAdoutqi  e                     inz('EMAILIN')
039500130227
039600130227       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
039700130227     d TRTCM1ds      e ds                  inz
039800130716         //?·§CM1mitt = Indirizzo e-mail del mittente?
039900130227     d   §CM1mitt    e                     inz(c_Mittente)
040000130716         //?·§CM1dst  = Indirizzo e-mail del destinatario?
040100130227     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
040200130716         //?·§CM1tips = Tipo lettera via e-mail:?
040300130716         // ?"LET" = testo allegato in corpo con logo?
040400130716         //         ?(richiede righe libere iniziali per il logo)?
040500130716         // ?"COR" = testo integrato senza logo?
040600130716         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
040700130716         // ?"SP1" = spool allegato (in formato PDF)?
040800130227     d   §CM1tips    e                     inz('COR')
040900130716         //?·§CM1po   = Sede?
041000130227     d   §CM1po      e                     inz(c_Sede)
041100130716         //?·§CM1var  = Oggetto e-mail?
041200130227     d   §CM1var     e                     inz('*OBJM*+
041300130227     d                                     Sped. elaborata oltre 100 +
041400130227     d                                     volte in FIILV - vedi log +
041500130227     d                                     "01"')
041600130716         //?·§CM1sts  = Stato?
041700130227     d   §CM1sts     e                     inz(*off)
041800130716         //?·§CM1idp  = Id processo?
041900130227     d   §CM1idp     e                     inz('2')
042000101220
042100101220       // -?Status ds?
042200030515     d Status         sds
042300120702     d   SDSpgm          *proc
042400120702     d   SDSprm          *parms
042500120702     d   SDSusr              254    263
042600101220
042700101220       // -?Parametro Numero Distinta?
042800101220     d ds_Distinta     ds             9    inz  qualified
042900101220     d   P_Fgs                        3s 0 inz
043000101220     d   P_Ndc                        6s 0 inz
043100080430
043200101207       // -?Parametro Numero Spedizione?
043300110207     d ds_Spedizione   ds            16    inz  qualified
043400080430     d   P_Aas                        4s 0 inz
043500080430     d   P_Lnp                        3s 0 inz
043600080430     d   P_Nrs                        2s 0 inz
043700080430     d   P_Nsp                        7s 0 inz
043800101220
043900080519     d BARcodeDS       ds            17    inz
044000120702     d   BARcdeAAS                    2s 0 inz
044100120702     d   BARcdeLNP                    3s 0 inz
044200120702     d   BARcdeNRS                    2s 0 inz
044300120702     d   BARcdeNSP                    7s 0 inz
044400101220
044500030515     d KEYpar          ds             8    inz
044600120702     d   WTPpar                1      1    inz
044700120702     d   WCDpar                2      3    inz
044800120702     d   Wprogr                4      8    inz('S0001')
044900101220
045000140303     d Elem_CP         ds                  inz
045100120702     d   WCPdcm                       8s 0 inz
045200140303     d   WCPocm                       4s 0 inz
045300120702     d   WCPncl                       5s 0 inz
045400140303     d   WCPfir                            inz  like(EVB2dati)
045500101213
045600101213       //--------------------------------------------------------------
045700101213       //?Definizione variabili globali.                               ?
045800101213       //--------------------------------------------------------------
045900101213
046000101220       // -?Parametri?
046100080430     d Spedizione      s             16
046200101217     d Distinta        s              9
046300101217     d DataOraCrea     s             14
046400080430     d Risultato       s              2
046500080715     d RootFolder      s            100
046600080519     d FileName        s             30
046700080731     d TemplateName    s             20
046800110201     d LastRow         s            120
046900131016     d RtnEsito        s             10i 0                                      // -?NON usato?
047000131016     d RtnOpCode       s             10                                         // -?NON usato?
047100131016     d RtnStatus       s             10i 0                                      // =?SQLcode?
047200080430     d P_Prt           s              1
047300101220
047400130214       // -?Flags booleani?
047500101220     d $Mamma          s               n   inz
047600130215     d $Mamma_FedEx    s               n   inz
047700101220     d $FIAR8          s               n   inz
047800130411     d $§PDF           s               n   inz
047900110204     d $FIILV          s               n   inz
048000101220     d $NoDesPartic    s               n   inz
048100110117     d*// $BolloVirt      s               n   inz(*off)
048200120113     d $No_x_Cons      s               n   inz
048300101220     d $ConsParz       s               n   inz
048400101220     d $CAeur          s               n   inz
048500100517     d $PreavvORM      s               n   inz
048600101220     d $GeoAtt         s               n   inz
048700131206     d $1o_Passaggio   s               n   inz
048800101220
048900101220       // -?Indici di schiera?
049000080523     d XX              s              3  0 inz
049100140304     d YY              s              3  0 inz
049200080523     d WX              s              3  0 inz
049300130313
049400130313       // -?Nome esteso Libreria/File del file TITAS30C?
049500130313     d wLibFileTITAS   s             21a   inz
049600080724
049700101207       // -?Campi di stampa?
049800130227     d P_Fgs2          s                   inz  like(ds_Distinta.P_Fgs)
049900130227     d P_Ndc2          s                   inz  like(ds_Distinta.P_Ndc)
050000101220
050100101220       // -?Campi di comodo alfanumerici?
050200030717     d w_CoOr          s             35    inz
050300081030     d wAWB            s             12    inz
050400030515     d w001a           s              1    inz
050500030515     d w008a           s              8    inz
050600030515     d w015a           s             15    inz
050700100517     d w053a           s             53    inz
050800070424     d wDateA          s              8    inz
050900070717     d wGeoTel         s              1    inz
051000070717     d wGeoCon         s              1    inz
051100080718     d wSysName        s              8    inz
051200130411      *? x TEST ?
051300130411     d $Test           s              3    inz
051400101220
051500101220       // -?Campi di comodo numerici?
051600030515     d w0080           s              8  0 inz
051700030624     d w0112           s             11  2 inz
051800030515     d w0140           s             14  0 inz
051900071105     d wDate           s              8  0 inz
052000090120     d w_Row           s              3  0 inz(c_Row)
052100110309     d wCountRec01     s              5  0 inz
052200101220
052300131008       // -?Campi utilizzati per impostazione dell'output?
052400110117     d*// wBolloVirt      s              4  2 inz
052500030515     d Wcas            s             13  3 inz
052600030515     d Wiva            s             11  2 inz
052700071015     d Wpdc4A          s              4    inz
052800030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
052900090619     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
053000101220
053100101220       // -?Campi utilizzati per il salvataggio momentaneo di dati?
053200040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
053300121018     d wB0cfi          s                   like(DB0cpi)   inz
053400121018     d wB0piv          s                   like(DB0cpi)   inz
053500120702     d wB0nrs          s                   like(ARBnrs)   inz
053600070514     d wB0ntc          s                   like(ARBntc)   inz
053700080523     d wB0dcm          s                   like(ARBdcm)   inz
053800070514     d wB0dcp          s                   like(ARBdcp)   inz
053900081120     d wB0tmc          s                   like(ARBtmc)   inz
054000130205     d wB0fiv          s                   like(AR6fiv)   inz
054100080506     d Wnsc            s                   like(ARTnsc)   inz
054200080506     d Wtrc            s                   like(AR4trc)   inz
054300030515     d Wctr            s                   like(DB0ctr)   inz
054400031124     d Wdsp            s                   like(DB0dft)   inz
054500090619     d WddtCLN         s                   like(DB0drt)   inz
054600090619     d WdspCLN         s              6  0                inz
054700030515     d Wsvx            s                   like(AR6sv1)   inz
054800030515     d Wvax            s                   like(AR6va1)   inz
054900110203     d wCod_Log        s                   like(ILVcol)   inz
055000090619
055100140303       //--------------------------------------------------------------
055200140303       //?Definizione procedure usate.                                 ?
055300090619       //--------------------------------------------------------------
055400100322
055500100322       // -?Procedure name: getCurrentSystemName?
055600100322       //  ?Purpose:        Reperisce l'attributo di rete current System Name?
055700100322       //  ?Returns:        Current System Name?
055800130716       //  ?(vedi "QWCRNETA")?
055900100322     d getCurrentSystemName...
056000100322     d                 pr             8a
056100100322
056200100322       //--------------------------------------------------------------
056300140303       //?Definizione procedure esterne.                               ?
056400100322       //--------------------------------------------------------------
056500100322
056600110201       // -?Calcolo check digit del barcode?
056700110201     d TRUL28ds      e ds                  inz
056800110201     d  I28tla       e                     inz('L')
056900110201     d  I28mod       e                     inz('BAR')
057000110201     d TrUl28r1        pr                  extpgm('TRUL28R1')
057100110201     d  trul28ds                           likeds(TRUL28ds)
057200110201
057300100322       // -?Calcolo tempi di delivery?
057400100322      /copy gaitrasrc/srcProtoPR,TNSD99R
057500130206
057600130206       // -?Ricerca/Controllo tabelle?
057700130206      /copy gaitrasrc/srcProtoPR,TIBS02R
057800100322
057900110201       // -?Parametri per API "QWCRNETA"?
058000110201      /copy QSYSINC/QRPGLESRC,QWCRNETA
058100110201     d qWcrNetA_dataReturned...
058200110201     d                 ds                  qualified inz
058300110201     d  qWcRDR                             likeds(qWCRDR)
058400110201     d  qWcOAT                       10i 0 dim(00001)
058500110201     d  qWcARRAY                     24
058600110201     d qWCRNETA_networkAttributeInformationTable...
058700110201     d                 ds                  qualified inz
058800110201     d  qWcRAT                             likeds(qWCRAT)
058900110201     d  qWcDATA                       8
059000100322       // -?Reperimento attributo di rete current System Name?
059100100322       //  ?(vedi "getCurrentSystemName")?
059200100322      /copy gaitrasrc/srcProtoPR,QWCRNETA
059300130227
059400130227       // -?Parametri API QCAPCMD (Process Commands)?
059500130227     d Qcmd            s           2048    inz  varying
059600130227      /copy qSysInc/qRpgleSrc,QCAPCMD
059700130227       // -?API QCAPCMD (Process Commands)?
059800130227      /copy gaitrasrc/srcProtoPR,QCAPCMD
059900130227
060000130227       // -?Parametri gestione errori API.?
060100130227      /copy qsysinc/qRpgleSrc,QUSEC
060200080710
060300080710       //--------------------------------------------------------------
060400140303       //?Definizione key-list.                                        ?
060500080710       //--------------------------------------------------------------
060600130313
060700130422       // -?File FIIRD11L?
060800130422     d k01fiird11    e ds                  extname(FIIRD11L : *key)
060900130313     d                                     prefix(k_)
061000130313     d                                     inz
061100130313
061200130313       // -?File TITAS30C?
061300130313     d k05titas30    e ds                  extname(TITAS30C : *key)
061400130313     d                                     prefix(k_)
061500130313     d                                     inz
061600131206
061700131206       // -?File FNEVB04L?
061800131206     d keyFNEVB04    e ds                  extname(FNEVB04L : *key)
061900131206     d                                     prefix(k_)
062000131206     d                                     inz
062100140303
062200140303       // -?File FNEVB22L?
062300140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
062400140303     d                                     prefix(k_)
062500140303     d                                     inz
062600170517
062700170517       // -?File TFNTC01L?
062800170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
062900170517     d                                     prefix(k_)
063000170517     d                                     inz
063100080710
063200101207       // -?File FIDST01L?
063300110228     d k03fidst01    e ds                  extname(FIDST01L : *key)
063400080710     d                                     prefix(k_)
063500080710     d                                     inz
063600090216
063700110202       // -?File FIILV01L?
063800110202     d k06fiilv01    e ds                  extname(FIILV01L : *key)
063900110207     d                                     prefix(k_)
064000090216     d                                     inz
064100030515
064200101220       //==============================================================
064300101220       //?RIEPILOGO INDICATORI                                         ?
064400101220       //--------------------------------------------------------------
064500101220       // 31    - di comodo
064600101220       // 35    - Porto Franco: segnala
064700101220       //         "PER RICEVUTA DELLA MERCE SENZA RISERVE"
064800101220       //?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia
064900101220       //         e indica "TOTALE DA ESIGERE AL RITIRO"
065000120524       // 70    - Trattasi di BOLLA BRT
065100101220       // 71    - Trattasi di BOLLA DPD
065200101220       // 72    - Trattasi di BOLLA FedEx
065300101220       // 73    - Trattasi di BOLLA EuroExpress
065400101220       //==============================================================
065500030515
065600101213       //--------------------------------------------------------------
065700101213       //?M A I N - L I N E                                            ?
065800101213       //--------------------------------------------------------------
065900101213
066000030515     c     *Entry        plist
066100080430     c                   parm                    Spedizione
066200101217     c                   parm                    Distinta
066300101217     c                   parm                    DataOraCrea
066400080430     c                   parm                    Risultato
066500080715     c                   parm                    RootFolder
066600080519     c                   parm                    FileName
066700080731     c                   parm                    TemplateName
066800110201     c                   parm                    LastRow
066900080430     c                   parm                    RtnEsito
067000080430     c                   parm                    RtnOpCode
067100080430     c                   parm                    RtnStatus
067200080430     c                   parm                    P_Prt
067300080430      *
067400110519      *?P_Prt:?non passato se richiamato da server, serve solo se?
067500110519      *       ?chiamato da menù AS400 x test?
067600101221      *   Valori: qualsiasi = richiamato da pgm. per test (stampa)
067700080430      *
067800070123      *
067900130924      * Operazioni iniziali
068000131016       //?SE rilevati errori nei parametri di input =>?
068100131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
068200070123     c                   exsr      sr_Start
068300030910      *
068400130924      * Pulizia dei campi di output e/o di lavoro
068500130924     c                   exsr      sr_ClrFLD
068600030515      *
068700130924      * Caricamento dei dati per l'immagine della LdV
068800131016       //?SE rileva errori nella distinta o nella spedizione =>?
068900131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
069000110208     c                   exsr      sr_CarARB
069100030515      *
069200130924      * Controllo del tipo LDV di cui generare l'immagine
069300110208     c                   exsr      sr_TipoLDV
069400030515      *
069500080430      * Impostazione dei campi di output con quelli della DS
069600110208     c                   exsr      sr_PrepPRT
069700030515      *
069800080430      * Decodifica dei dati della LDV
069900110208     c                   exsr      sr_DecodLDV
070000080806      *
070100080806      * Impostazione dati per immagine LdV
070200131016     c                   exsr      sr_DatiOUT
070300030515      *
070400030515      * Chiusura pgm
070500080430     c                   exsr      sr_End
070600030515      *
070700030515      *---------------------------------------------------------------*
070800050217      *?OPERAZIONI INIZIALI                                          ?*
070900030515      *---------------------------------------------------------------*
071000030515     c     *InzSR        BEGSR
071100070424      *
071200070424      * Reperisco la data del giorno
071300070424     c                   time                    w0140
071400070424     c                   move      w0140         w0080
071500070424     c     *eur          movel     w0080         Data_EUR
071600070424     c     *iso          movel     Data_EUR      w0080
071700070424     c                   move      w0080         wDateA
071800071105     c                   eval      wDate  = w0080
071900030515      *
072000030515      * Imposto i campi chiave "fissi"
072100040223     c                   eval      TBLkut = 1
072200100901     c                   eval      ARStrc = 'C'
072300030515      *
072400040617      * Impostazione dati in KPJBA
072500070308     c                   eval      KNMUS  = SDSusr
072600030515      *
072700030515      * Reperisco la moneta di conto
072800030515     c                   clear                   dGED
072900040607     c                   clear                   TIBS02ds
073000040607     c                   eval      T02mod = 'C'
073100040607     c                   eval      T02cod = 'GED'
073200040607     c                   eval      T02ke1 = '1         '
073300030515     c                   call      'TIBS02R'
073400030515     c                   parm                    KPJBA
073500040607     c                   parm                    TIBS02ds
073600030515if  1c                   if        T02err = *blanks
073700030515     c                   movel     T02uni        dGED
073800030515e   1c                   endif
073900061031      *
074000131008      * Reperisco data di decorrenza indicaz. "Per ricevuta di ...
074100131008      *                                        senza riserve"
074200131008      *              e di decorrenza indicaz. "Consegna al mattino"
074300061031     c                   clear                   dVPOdeco
074400061031     c                   clear                   TIBS02ds
074500061031     c                   eval      T02mod = 'C'
074600071112     c                   eval      T02cod = 'VPO'
074700061031     c                   eval      T02ke1 = 'DECO      '
074800061031     c                   call      'TIBS02R'
074900061031     c                   parm                    KPJBA
075000061031     c                   parm                    TIBS02ds
075100061031if  1c                   if        T02err = *blanks
075200061031     c                   movel     T02uni        dVPOdeco
075300061031e   1c                   endif
075400030515      *
075500030515     c                   ENDSR
075600070123      *
075700101220      /free
075800101220
075900101220       //--------------------------------------------------------------
076000101220       //?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?
076100101220       //--------------------------------------------------------------
076200101220       BEGSR  sr_Start;
076300101220
076400101220         exec sql  set option  dynusrprf = *owner,
076500101220                               closqlcsr = *endmod;
076600101220
076700101221         *inLR = *on;
076800080509
076900110517         // -?Impostazione iniziale dei dati di ritorno?
077000080509         Risultato = 'OK';
077100110202         clear  RootFolder;
077200110202         clear  FileName;
077300120524         //TemplateName = 'ldvbase00 ';   //?Standard - BARTOLINI senza box aggiuntivo?
077400120524         //TemplateName = 'ldvbase10 ';   //?Standard - BARTOLINI-BRT senza box aggiuntivo?
077500130924         TemplateName = 'ldvbase20 ';     //?Standard - BRT senza box aggiuntivo?
077600110202         clear  LastRow;
077700110202         clear  RtnEsito;
077800110202         clear  RtnOpCode;
077900110202         clear  RtnStatus;
078000110207
078100110207         // -?Apertura archivio di output - comunque utilizzato?
078200110207         if  NOT  %open(FIILV01L);
078300110207           open(e)  FIILV01L;
078400110207           if  %error;
078500110207             exsr  sr_ErrOpenFile;
078600110207           endif;
078700110207         endif;
078800110207
078900110202         clear  k06fiilv01;
079000110203         clear  FIILV000;
079100080509
079200110203         // »? STEP 0 ?«
079300131016         // -?Controllo parametri ricevuti.  SE rileva errori =>?
079400131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
079500130716         exsr  sr_STEP_0_CtrlParms;
079600110203
079700080430
079800101213         // -?Impostazione spedizione ricevuta come parametro?
079900080430         ds_Spedizione = Spedizione;
080000110126         // -?Impostazione distinta ricevuta come parametro?
080100110126         ds_Distinta = Distinta;
080200080430
080300101213         // -?Impostazione iniziale dei dati nella ds FNLSB5DS?
080400130924         //clear FNLSB5ds;            ?(già così)?
080500080430         DB0tbo = 'A';
080600110202         DB0aas = ds_Spedizione.P_Aas;
080700110202         DB0lnp = ds_Spedizione.P_Lnp;
080800110202         DB0nrs = ds_Spedizione.P_Nrs;
080900110202         DB0nsp = ds_Spedizione.P_Nsp;
081000080519
081100101213         // -?Impostazione campo di output "identificativo spedizione"?
081200080519         clear  BarCodeDS;
081300080519         BARcdeAAS = %int( %subst( %editc( DB0aas : 'X' ) : 3 : 2) );
081400080519         BARcdeLNP = DB0lnp;
081500080519         BARcdeNRS = DB0nrs;
081600080519         BARcdeNSP = DB0nsp;
081700080519         FileName  = BarCodeDS;
081800130313
081900130716         // -?Impostazione chiave di accesso al file FIIRD11L?
082000130313         k_IRDfgsndc = Distinta;
082100130313
082200130313         // -?Impostazione chiave di accesso al file TITAS30C?
082300130313         clear  k05titas30;
082400130313         k_TASaas = DB0aas;
082500130313         k_TASlnp = DB0lnp;
082600130313         k_TASnrs = DB0nrs;
082700130313         k_TASnsp = DB0nsp;
082800131206
082900131206         // -?Impostazione chiave di accesso al file FNEVB04L?
083000131206         clear  keyFNEVB04;
083100131206         k_EVBaas = DB0aas;
083200131206         k_EVBlnp = DB0lnp;
083300131206         k_EVBnrs = DB0nrs;
083400131206         k_EVBnsp = DB0nsp;
083500131210         //k_EVBcev = 'MIC';          ?(leggere TUTTI i rec.)?
083600140303
083700140303         // -?Impostazione chiave di accesso al file FNEVB22L?
083800140303         clear  keyFNEVB22;
083900140303         k_EVB2aas = DB0aas;
084000140303         k_EVB2lnp = DB0lnp;
084100140303         k_EVB2nrs = DB0nrs;
084200140303         k_EVB2nsp = DB0nsp;
084300140303         //k_EVB2dtv = .....;
084400140303         //k_EVB2orv = .....;
084500140303         //k_EVB2cev = 'P  ';
084600110204
084700110204         // -?Impostazione chiave di accesso al file FIILV01L?
084800110204         k_ILVaas = DB0aas;
084900110204         k_ILVlnp = DB0lnp;
085000110204         k_ILVnrs = DB0nrs;
085100110204         k_ILVnsp = DB0nsp;
085200110225         k_ILVfgs = ds_Distinta.p_Fgs;
085300110225         k_ILVndc = ds_Distinta.p_Ndc;
085400120702         clear  dILVlibero;
085500130411
085600130411
085700130411         IF  '1' <> '1';
085800130411
085900130924           // -? CHIODI MOMENTANEI PER TEST                                 ?
086000130924           //  ? da attivare per ricavare immagini di esempio forzando sped ?
086100130924           clear  $Test;
086200130411           Select;
086300130411             // -?Spedizione FedEx?
086400130411             When  Spedizione = '2011050001843581' and Distinta = '145040335';
086500130411               $Test = 'FED';
086600130411             // -?Spedizione DPD?
086700130411             When  Spedizione = '2011190000579210' and Distinta = '145040339';
086800130411               $Test = 'DPD';
086900130411             // -?Spedizione con Consegna Parziale Precedente?
087000130411             When  Spedizione = '2011001000816011' and Distinta = '145040361';
087100130411               $Test = 'C_P';
087200130411             // -?Spedizione Normale?
087300130411             When  Spedizione = '2011163000042281' and Distinta = '145040333';
087400130411               $Test = 'NOR';
087500130411             // -?Altra spedizione: NON ELABORABILE?
087600130411             Other;
087700130411               Risultato = 'NO';
087800130924               //RtnOpCode = 'NO_IN_TEST';
087900130924               //RtnEsito  = -1;
088000130924               // -?Uscita dalla s.p.?
088100130411               exsr  sr_End;
088200130411           EndSl;
088300130411
088400130411         ENDIF;
088500130411
088600080430
088700110208         // -?Apertura archivi di input - comunque utilizzati?
088800110207         if  NOT  %open(TABEL00F);
088900110207           open(e)  TABEL00F;
089000110207           if  %error;
089100110207             exsr  sr_ErrOpenFile;
089200110207           endif;
089300110207         endif;
089400110207
089500110207         if  NOT  %open(AZORG01L);
089600110207           open(e)  AZORG01L;
089700110207           if  %error;
089800110207             exsr  sr_ErrOpenFile;
089900110207           endif;
090000110207         endif;
090100110207
090200110207         if  NOT  %open(FNARB01L);
090300110207           open(e)  FNARB01L;
090400110207           if  %error;
090500110207             exsr  sr_ErrOpenFile;
090600110207           endif;
090700110207         endif;
090800110207         if  NOT  %open(FIAR601L);
090900110207           open(e)  FIAR601L;
091000110207           if  %error;
091100110207             exsr  sr_ErrOpenFile;
091200110207           endif;
091300110207         endif;
091400110207         if  NOT  %open(FIAR701L);
091500110207           open(e)  FIAR701L;
091600110207           if  %error;
091700110207             exsr  sr_ErrOpenFile;
091800110207           endif;
091900110207         endif;
092000110207         if  NOT  %open(FIAR901L);
092100110207           open(e)  FIAR901L;
092200110207           if  %error;
092300110207             exsr  sr_ErrOpenFile;
092400110207           endif;
092500110207         endif;
092600110207         if  NOT  %open(FIDST01L);
092700110207           open(e)  FIDST01L;
092800110207           if  %error;
092900110207             exsr  sr_ErrOpenFile;
093000110207           endif;
093100110207         endif;
093200101213
093300101213         if  NOT  %open(FIAR401L);
093400101213           open(e)  FIAR401L;
093500101213           if  %error;
093600101213             exsr  sr_ErrOpenFile;
093700101213           endif;
093800101213         endif;
093900101213         if  NOT  %open(FIAR501L);
094000101213           open(e)  FIAR501L;
094100101213           if  %error;
094200101213             exsr  sr_ErrOpenFile;
094300130411           endif;
094400130411         endif;
094500101213
094600101220       ENDSR;
094700101220
094800101213       //--------------------------------------------------------------
094900101213       //?Restituzione errore in fase di apertura di un file           ?
095000101213       //--------------------------------------------------------------
095100101213       BEGSR  sr_ErrOpenFile;
095200101213
095300101213         Risultato = 'NO';
095400130924         //RtnOpCode = 'ERR_OPEN  ';
095500101213         //RtnEsito  = -1;
095600101213
095700130924         // -?Uscita dalla s.p.?
095800101213         exsr  sr_End;
095900101213
096000101213       ENDSR;
096100130716
096200130716       //--------------------------------------------------------------
096300130716       //?»  STEP 0  «                                                 ?
096400130716       //?Controllo dei parametri ricevuti, reperimento nome del       ?
096500130716       //?sistema e dei percorsi per le immagini in base ad esso.      ?
096600130716       //--------------------------------------------------------------
096700130716       BEGSR  sr_STEP_0_CtrlParms;
096800130716
096900130716         //   _________________________________________________________
097000130716         // -? Controllo lunghezza e numericità dei parametri ricevuti ?
097100130716         //   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
097200130716         SELECT;
097300130716
097400130716           // -?NUMERO SPEDIZIONE?
097500130716           WHEN  ( %len( %trimr( Spedizione ) ) )  <> 16     or
097600130716                 ( %check (c_Digits : Spedizione) ) > *zero  or
097700130716                                      Spedizione   <= *zero;
097800130716             Risultato = 'XX';
097900130924             //RtnOpCode = 'ERR_NUMSPE';
098000130716             //RtnEsito  = -1;
098100130716             // -?Registrazione log in FIILV?
098200130716             wCod_Log = '21';
098300130716             exsr  sr_WrtFIILV;
098400130716             // -?Uscita dalla s.p.?
098500130716             exsr  sr_End;
098600130716
098700130716           // -?NUMERO DISTINTA?
098800130716           WHEN  ( %len( %trimr( Distinta ) ) )  <> 9      or
098900130716                 ( %check (c_Digits : Distinta) ) > *zero  or
099000130716                                      Distinta   <= *zero;
099100130716             Risultato = 'XX';
099200130924             //RtnOpCode = 'ERR_DISTIN';
099300130716             //RtnEsito  = -1;
099400130716             // -?Registrazione log in FIILV?
099500130716             wCod_Log = '21';
099600130716             exsr  sr_WrtFIILV;
099700130716             // -?Uscita dalla s.p.?
099800130716             exsr  sr_End;
099900130716         ENDSL;
100000130716
100100130716         // ________________________________________________________
100200130716         //? Reperimento nome del sistema per impostazione percorso ?
100300130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
100400130716         wSysName = getCurrentSystemName();
100500130716         if  wSysName = *blank;
100600130716           Risultato  = 'NO';
100700130924           //RtnOpCode  = 'SYS_ERR   ';
100800130924           //RtnEsito   = -1;
100900130924           // -?Uscita dalla s.p.?
101000130716           exsr  sr_End;
101100130716         endif;
101200130716
101300130716         // ______________________________________________________
101400130716         //? Reperimento dei percorsi in base al nome del sistema ?
101500130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
101600130716         clear  dILV;
101700130716         reset  TIBS02ds;
101800130716         T02cod = 'ILV';
101900130716         T02ke1 = %subst( wSysName : 1 : 6 );
102000130716         T02ke2 = 'LINEA';
102100130716         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
102200130716         if  T02err = *blank;
102300130716           dILV = T02uni;
102400130716         endif;
102500130716
102600130716       ENDSR;
102700110208
102800110208       //--------------------------------------------------------------
102900130924       //?Pulizia dei campi di output e/o di lavoro                    ?
103000110208       //--------------------------------------------------------------
103100130924       BEGSR  sr_ClrFLD;
103200080430
103300081021         clear ds_01;
103400101213       //clear ds_02;                      ?(riga vuota)?
103500080430         clear ds_03;
103600080430         clear ds_04;
103700080430         clear ds_05;
103800101213       //clear ds_06;                      ?(riga vuota)?
103900080430         clear ds_07;
104000101213       //clear ds_08;                      ?(riga vuota)?
104100080430         clear ds_09;
104200080430         clear ds_10;
104300080430         clear ds_11;
104400080430         clear ds_12;
104500080430         clear ds_13;
104600080430         clear ds_14;
104700130205         clear ds_140;
104800080430         clear ds_15;
104900080430         clear ds_16;
105000080430         clear ds_17;
105100080430         clear ds_18;
105200130205         clear ds_180;
105300080430         clear ds_19;
105400080430         clear ds_20;
105500080430         clear ds_21;
105600080430         clear ds_22;
105700080430         clear ds_23;
105800080430         clear ds_24;
105900080430         clear ds_25;
106000101213       //clear ds_26;                      ?(vuoto)?
106100080430         clear ds_27;
106200080430         clear ds_28;
106300081021         clear ds_29;
106400081021         clear ds_30;
106500110215         clear ds_31;
106600081021         clear ds_32;
106700081021         clear ds_33;
106800081021         clear ds_34;
106900081023         clear ds_37;
107000081030         clear ds_Fed;
107100080430
107200080430      /end-free
107300030515      *
107400060524     c                   clear                   dsBL4A
107500061030     c                   clear                   dsBL4I
107600110204     c                   clear                   dsBL4M
107700100517     c                   clear                   dOrm01
107800041207      *
107900070514     c                   reset                   $Mamma
108000130215     c                   reset                   $Mamma_FedEx
108100060921     c                   reset                   $FIAR8
108200130411     c                   reset                   $§PDF
108300110204     c                   reset                   $FIILV
108400030710     c                   reset                   $NoDesPartic
108500110117     c*//                reset                   $BolloVirt
108600040707     c                   reset                   $No_x_Cons
108700070319     c                   reset                   $ConsParz
108800030515     c                   reset                   $CAeur
108900100901     c                   reset                   $PreavvORM
109000101220     c                   reset                   $GeoAtt
109100070717     c                   clear                   wGeoTel
109200070717     c                   clear                   wGeoCon
109300070717     c                   clear                   Wdsp
109400090619     c                   clear                   WddtCLN
109500090619     c                   clear                   WdspCLN
109600030717     c                   clear                   W_CoOr
109700110117     c*//                clear                   wBolloVirt
109800030515     c                   clear                   Wcas
109900030515     c                   clear                   Wiva
110000071015     c                   clear                   Wpdc4a
110100121018     c                   clear                   wB0cfi
110200121018     c                   clear                   wB0piv
110300031015     c                   clear                   wB0nrs
110400070514     c                   clear                   wB0ntc
110500080523     c                   clear                   wB0dcm
110600070514     c                   clear                   wB0dcp
110700081120     c                   clear                   wB0tmc
110800130205     c                   clear                   wB0fiv
110900030515      *
111000030515     c                   clear                   $NP
111100070319     c                   reset                   $CP
111200070319     c                   reset                   $CPdcm
111300030515      *
111400030515     c                   movea     *zeros        *in(01)
111500030515      *
111600030515     c                   ENDSR
111700030515      *
111800030515      *---------------------------------------------------------------*
111900131008      *?Determino il modulo LDV da usare per l'immagine              ?*
112000131008      *?(se DPD, FedEx o BRT - con consegne parziali precedenti o    ?*
112100131008      *? promrmoria per l'autotrasportatore).                        ?*
112200030515      *---------------------------------------------------------------*
112300110208     c     sr_TipoLDV    BEGSR
112400070215      *
112500070212      * Impostazione della data spedizione
112600110208      * (se non già impostata in subr. "sr_CarARB")
112700070212if  1c                   if        Wdsp   =  *zeros
112800070212if  2c                   if        DB0mgs <> *zeros
112900070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
113000070212x   2c                   else
113100070212      *   prendo la data del giorno se non c'è la data spedizione
113200070212     c                   time                    w0140
113300070212     c                   move      w0140         w0080
113400070212     c     *eur          movel     w0080         Data_EUR
113500070212     c     *iso          movel     Data_EUR      Wdsp
113600070212e   2c                   endif
113700070212e   1c                   endif
113800040607      *
113900070308      * Dati della filiale del mittente
114000040607     c                   eval      WX     =  DB0ccm / 10000
114100040607if  1c                   if        WX     <> ORGfil
114200040607     c     WX            chain     AZORG
114300040607e   1c                   endif
114400040607if  1c                   if        %found(AZORG01L)
114500040607     c                   movel     ORGde3        OG143ccm
114600040607x   1c                   else
114700040607     c                   clear                   OG143ccm
114800040607e   1c                   endif
114900040311      *
115000040311      * Dati della linea di arrivo
115100131203      *?(SONO GIÀ STATI REPERITI NELLA SUBR. "SR_DECLNA",?
115200131203      *? ESEGUITA DALLA SUBR. "SR_CARARB")?
115300070212      *
115400070212      * Dati della capofila della linea di arrivo
115500131205      * - per verificare TELLUS
115600070717      *
115700070717      *** Imposta flag attivazione GEO x LNA
115800070717     c                   eval      wGeoCon = A§OGtlls
115900070717     c                   eval      wGeoTel = A§OGgeot
116000070717if  1c                   if             A§OGdda  > *zeros
116100070717     c                             and  A§OGdda  < C_DateMax_A
116200070717     c                             and  A§OGdda <= wDateA
116300101220     c                   eval      $GeoAtt = *on
116400070717e   1c                   endif
116500070717      *
116600110203if  1c                   IF             wGeoCon <> 'S'
116700070717     c                              or  wGeoTel <> 'S'
116800110203     c                              or  $GeoAtt <> *on
116900070212      * - Reperimento capofila (£6)
117000070212if  2c                   if        DB0lna <> D55lin
117100070212     c                   reset                   FNLV55ds
117200130924     c***?già così:?     eval      D55tpt =  '6'
117300070212     c                   eval      D55lin =  DB0lna
117400070212     c                   eval      D55drf =  Wdsp
117500070212     c                   call      'FNLV55R'
117600070212     c                   parm                    FNLV55ds
117700070212if  3c                   if        D55err <> *blanks
117800070212     c                   eval      D55tfa =  DB0lna
117900070212e   3c                   endif
118000070212e   2c                   endif
118100070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
118200070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
118300070212if  2c                   if        D55tfa <> ORGfil
118400070212     c     D55tfa        chain     AZORG
118500071109if  3c                   if        %found(AZORG01L)
118600070717     c                   movel     ORGde6        OG146cf£6
118700070717     c                   movel     ORGde8        OG148cf£6
118800071109x   3c                   else
118900071109     c                   clear                   OG146cf£6
119000071109     c                   clear                   OG148cf£6
119100071109e   3c                   endif
119200071109e   2c                   endif
119300070717      *** Imposta flag attivazione GEO x capofila £6
119400070717     c                   eval      wGeoCon = £6OGtlls
119500070717     c                   eval      wGeoTel = £6OGgeot
119600071109if  2c                   if             £6OGdda  > *zeros
119700070717     c                             and  £6OGdda  < C_DateMax_A
119800070717     c                             and  £6OGdda <= wDateA
119900101220     c                   eval      $GeoAtt = *on
120000071109e   2c                   endif
120100070717      *
120200070212e   1c                   ENDIF
120300040607      *
120400130411      * Reperimento dati da FIAR5 / "GEN"
120500130411      *   (per verificare se esistono Packing-List e/o PDF allegati
120600130411      *    - oltre che per reperire Nominativo e Telefono)
120700130411     c                   exsr      RepAR5gen
120800130411      *
120900130411      * Verifica se LDV con DDT e/o PDF del mittente
121000130411     c                   exsr      CercaDDToPDF
121100131206      *
121200131206      * Controllo se già effettuato il 1° passaggio c/o destinatario
121300131206     c                   exsr      sr_Check1pass
121400070319      *
121500070319      * Verifica se richiesta spedizione già parzialmente consegnata
121600070319      *   (da stampare nel box "Note per l'autotrasportatore")
121700070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
121800070717      * completamente
121900110203if  1c                   if             wB0dcp   >  *zeros
122000101220     c                             and  $GeoAtt  =  *on
122100070413     c                   eval      $ConsParz     =  *on
122200070319e   1c                   endif
122300030515      *
122400040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
122500040607      *   riserve)
122600120524      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
122700040223     c                   movea     '1000'        *in(70)
122800040311      *
122900030515sel 1c                   SELECT
123000071213      *
123100170517      * LDV DPD?(dalla fil. del mittente, NON della LNP)?
123200071213w   1c                   WHEN      §CMntw = 'DPD'
123300071213     c                   movea     '0100'        *in(70)
123400071213      *
123500071213      * LDV con mittente FedEx
123600150430w   1c                   WHEN      DB0ccm = c_CliFedEx   or
123700150430     c                             DB0ccm = c_CliFedEx2
123800071213     c                   movea     '0010'        *in(70)
123900030515      *
124000030515e   1c                   ENDSL
124100040707      *
124200040707      * Controllo se devo stampare "Non utilizzare per la consegna"
124300040707      * nella copia per il vettore,?senza?copia per il destinatario:
124400130924     c***?(già fatto:)?  reset                   $No_x_Cons
124500071203      * - LdV recapito c/assegni:
124600100322      *   stampata in arrivo e con
124700120113if  1c                   if             DB0lnp =  102
124800070306     c                             and  DB0cbo =  'FW'
124900071203     c                             and  DB0nft =  *zeros
125000071203      *   - fil. arrivo con GEO attivato
125100071203     c                             and (DB0fl1    =  'A'
125200071203     c                              or  wGeoCon   =  'S'
125300071203      *   - fil. arrivo abilitata al picking
125400120113     c                              or  A§OGpck   =  'S')
125500070306     c                   eval      $No_x_Cons  =  *on
125600120113e   1c                   endif
125700030515      *
125800030515     c                   ENDSR
125900040607      *
126000040607      *---------------------------------------------------------------*
126100130411      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
126200130411      *?               di uno o più PDF allegati da stampare         ?*
126300040607      *---------------------------------------------------------------*
126400130411     c     CercaDDToPDF  BEGSR
126500040607      *
126600130924     c***?(già fatto:)?  reset                   $FIAR8
126700130924     c***?(già fatto:)?  reset                   $§PDF
126800040607      *
126900130924     c***?(già fatto:)?  exsr      RepAR5gen
127000130411      *
127100130411      * -?Verifica esistenza PDF allegato?
127200130411     c                   eval      $§PDF  = (§AR5als = 'S')
127300130411      *
127400130411      * -?Verifica esistenza packing-list?
127500130411     c                   eval      $FIAR8 = (§AR5alx = 'S')
127600040607      *
127700080505     c                   ENDSR
127800131206      *
127900131206      /free
128000131206
128100131206       //--------------------------------------------------------------
128200131206       //?Verifica se già effettuato il 1° passaggio.                  ?
128300131206       //--------------------------------------------------------------
128400131206       BEGSR  sr_Check1pass;
128500131206
128600131206         //reset  $1o_Passaggio       ?(già fatto)?
128700131206
128800131206         // -?Apertura archivio?
128900131206         If  NOT  %open(FNEVB04L);
129000131206           open(e)  FNEVB04L;
129100131206           if  %error;
129200131206             exsr  sr_ErrOpenFile;
129300131206           endif;
129400131206         EndIf;
129500131206
129600131206         // -?Ciclo di lettura per ricercare l'eventuale 1° passaggio?
129700131210         setgt   %kds(keyFNEVB04 : 4)  FNEVB000;
129800131210         readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
129900131206
130000131206         DoW  Not %eof(FNEVB04L)  and  Not $1o_Passaggio;
130100131206
130200131206           dEVB01 = EVBnot;
130300131206           $1o_Passaggio = (dEVB01.§NOTprg = 'P');
130400131206
130500131206           if  Not $1o_Passaggio;
130600131210             readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
130700131206           endif;
130800131206
130900131206         EndDo;
131000131206
131100131206       ENDSR;
131200131206
131300131206      /end-free
131400030515      *
131500030515      *---------------------------------------------------------------*
131600050217      *?Imposta i campi di stampa con quelli della DS                ?*
131700030515      *---------------------------------------------------------------*
131800110208     c     sr_PrepPRT    BEGSR
131900040223      *
132000040223      * Reperimento dati da FIAR5 / "GEN"
132100130924     c***?(già fatto:)?  exsr      RepAR5gen
132200070212     *** *
132300070212     *** * Impostazione Data spedizione
132400110208     *** * (già fatto nella subr. "sr_TipoLdV")
132500030515      *
132600030515      * - Intestazione moduli
132700030515      *
132800030515      * - Sigla operatore video
132900030515     c                   eval      Psop   =  DB0sop
133000081120      /free
133100081120         //
133200130411         // -?Flag "allega documento di trasporto"?
133300081120         select;
133400081120
133500130411           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
133600081120           when  DB0bam =  'S'   and   wB0tmc <> *blank;
133700081120             evalr Pddt =  'DDT sul collo';
133800081120
133900130411           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
134000081120           when  DB0bam =  'S';
134100081120             evalr Pddt =  'DDT';
134200081120
134300130716           // -?L'indicazione di packing-list è stata spostata in PPKL?
134400130716           //// -?Esiste packing-list abbinata?
134500130411           //when  $FIAR8 =  *on   and   $§PDF =  *off;
134600131203           //  evalr Pddt =  'PACKING-LIST';
134700130716           //// -?LdV con PDF allegato?
134800130411           //when  $FIAR8 =  *off  and   $§PDF =  *on;
134900130411           //  evalr Pddt =  'PACKING-LIST PDF';
135000130716           //// -?LdV con packing-list abbinata & PDF allegato?
135100130411           //when  $FIAR8 =  *on   and   $§PDF =  *on;
135200130411           //  evalr Pddt =  'PACKING-LIST PDF';
135300130411
135400130411           // -?LdV senza PDF allegato & senza packing-list abbinata?
135500081120           other;
135600081120             clear Pddt;
135700081120
135800081120         endsl;
135900081120
136000081120      /end-free
136100030515      *
136200030515      * - Mittente
136300170517     c                   if        *in71
136400170517     c                   exsr      sr_Mitt_DPD
136500170517     c                   endif
136600170517      *
136700030515     c                   if        DB0ccm <> *zeros
136800030515     c                   move      DB0ccm        Pccm
136900030515     c                   endif
137000030515     c                   eval      Prsm   =  DB0rsm
137100030515     c                   eval      Pinm   =  DB0inm
137200030515     c                   eval      Pcam   =  DB0cam
137300030515     c                   eval      Plom   =  DB0lom
137400030515     c                   eval      Pprm   =  DB0prm
137500030515     c                   eval      Pnzm   =  DB0nzm
137600030515      *   Mittente originale
137700030515if  1c                   if        DB0rmo <> *blanks
137800080430     c                   eval      Pkmo   =  'Mitt.Originale '
137900030515     c                   eval      Prmo   =  DB0rmo
138000030515e   1c                   endif
138100030515      *
138200070308      * - Filiale di Arrivo
138300030515     c                   move      DB0lna        Plna
138400030515      *
138500030515      * - Destinatario
138600030515if  1c                   if        DB0ccd <> *zeros
138700030515     c                   move      DB0ccd        Pccd
138800030515e   1c                   endif
138900030515     c                   eval      Prsd   =  DB0rsd
139000030515     c                   eval      Prse   =  DB0rd2
139100030515     c                   eval      Pind   =  DB0ind
139200030515     c                   eval      Pcad   =  DB0cad
139300030515     c                   eval      Plod   =  DB0lod
139400030515     c                   eval      Pprd   =  DB0prd
139500030515     c                   eval      Pnzd   =  DB0nzd
139600030515      *
139700030515      * - Codice tariffa
139800030515if  1c                   if        %subst(DB0tb1:1:1) = 'A'
139900030515      *   - nel box del destinatario se porto assegnato
140000030515     c                   move      DB0ctr        Pcta
140100030515x   1c                   else
140200030515      *   - nel box del mittente se porto franco
140300030515     c                   move      DB0ctr        Pctf
140400030515e   1c                   endif
140500030515      *
140600030515      * - Riferimenti mittente
140700080430     c                   evalr     Prmn   =  %char(DB0rmn)
140800030515     c                   eval      Prma   =  DB0rma
140900061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
141000130924     c***?(già fatto:)?  clear                   dsBL4I
141100060626     c                   if        *in71
141200061103     c                   exsr      RepAR4_I
141300060626     c                   endif
141400030515      *
141500030515      * - Nominativo & telefono
141600031124      *   (da FIAR5 / "GEN")
141700130924     c***?(già fatto:)?  exsr      RepAR5gen
141800031124     c                   eval      Pnom   =  §AR5ref
141900031124     c                   eval      Ptel   =  §AR5teld
142000030515      *
142100030515      * - Spedizione
142200030515      *   (numero: non se Rimesso al Mittente)
142300030515if  1c                   if        DB0ris <> 'M'
142400080430     c                   eval      Plnp   =  %editc(DB0lnp : 'X')
142500080430     c                   eval      Pnrs   =  %editc(DB0nrs : 'Z')
142600080430     c                   eval      Pnsp   =  %editc(DB0nsp : 'Z')
142700030515e   1c                   endif
142800030515      * . . Data spedizione
142900031124     c     *iso          movel     Wdsp          Data_EUR
143000080506     c                   eval      PdspA = %editc( %dec(Data_EUR) : 'Y' )
143100030515      *
143200030515      * - Colli: numero, peso, volume, flag tipo-volume
143300080430     c                   eval      PnclA  =  %editc(DB0ncl : 'Z')
143400080911      /free
143500080911           if  NOT *in71;
143600080911      /end-free
143700080430     c                   eval      PpkfA  =  %editc(DB0pkf : '4')
143800080430     c                   eval      PvlfA  =  %editc(DB0vlf : '4')
143900030515if  1c                   if            DB0fvf <> *blanks
144000030515     c                             and DB0ris <> 'M'
144100081021     c                   movel     DB0fvf        Pfvf
144200030515e   1c                   endif
144300080915      /free
144400080915           endif;
144500080915      /end-free
144600030515      *
144700030515      * - Natura merce
144800030515     c                   eval      Pnat   =  DB0nas
144900030515      *
145000030515      * - Servizio
145100100517      /free
145200100517         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
145300110117           exsr  sr_CheckORM_PreavvMail;
145400100517         endif;
145500100517      /end-free
145600100517      * . . Descrizione porto
145700130925      *     (NON se presente ORM con preavviso mail)
145800100517     c                   if        Not  $PreavvORM
145900030515     c                   eval      Pds1   =  DB0dcb
146000100517     c                   endif
146100030515      * . . Descrizione tipo servizio
146200030515if  1c                   if        DB0fst =  'S'
146300030515     c                   eval      Pds2   =  DB0dts
146400030515e   1c                   endif
146500030515      * . . Reso / Franco frontiera
146600030515sel 1c                   select
146700030515w   1c                   when      DB0fbr =  'S'
146800030515     c                   clear                   w008a
146900030515     c                   eval      w008a  =  '   RESO '
147000030515     c                   move      w008a         Pds2
147100030515w   1c                   when      DB0fbr =  'F'
147200030515     c                   clear                   w015a
147300030515     c                   eval      w015a  =  '   FR.FRONTIERA'
147400030515     c                   move      w015a         Pds2
147500030515e   1c                   endsl
147600030515      *
147700080430      * - Zona consegna
147800080430      *   (campo alfanumerico)
147900080430     c                   eval      Pznc   =  %editc(DB0znc : 'X')
148000030515      *
148100030515      * - Flag "*" (gestito a parte)
148200040223      *
148300040223      * - Flag "V" = colli di valore
148400040223if  1c                   if        §AR5bva   <> *blank
148500040223     c                   eval      Pbva   =  'V'
148600040223e   1c                   endif
148700030515      *
148800030515      * - Fermo deposito
148900030515if  1c                   if        DB0ffd <> *blanks
149000080430     c                   eval      Pgc2   =  'FERMO DEPOSITO '
149100030515e   1c                   endif
149200030515      *
149300070308      * - Segnacolli: filiale, serie, dal / al
149400030515if  1c                   if        DB0fls <> *zeros
149500080430     c                   eval      Ppos   =  %editc(DB0fls : 'X')
149600030515e   1c                   endif
149700040607      * - - se bolla con mamma considero la serie della mamma
149800070515if  1c                   if        $Mamma =  *on
149900080430     c                   eval      Psrs   =  %editc(wB0nrs : 'Z')
150000031015x   1c                   else
150100080430     c                   eval      Psrs   =  %editc(DB0nrs : 'Z')
150200031015e   1c                   endif
150300030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
150400030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
150500030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
150600030515      *   Se collo singolo .......:   "     SCD
150700031015sel 1c                   select
150800030515      * - - non sequenziali
150900030515w   1c                   when      DB0fns =  'S'
151000131008if  2c                   if        DB0ncl <= 2
151100050511     c                   exsr      ListaScNSd
151200131008x   2c                   else
151300081021     c                   eval      Pnsx   =  'NON SEQUENZIALI'
151400131008e   2c                   endif
151500030515      * - - singolo o sequenziali
151600030515      *     (non se rimesso al mittente)
151700030515w   1c                   when      DB0ris <> 'M'
151800080430     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
151900030515if  2c                   if            DB0nca <> *zeros
152000030515     c                             and DB0nca <> DB0ncd
152100080430     c                   eval      Pnsx   =  %trimr(Pnsx) + ' - '
152200080430     c                                    +  %editc(DB0nca : 'Z')
152300030515e   2c                   endif
152400030515e   1c                   endsl
152500030515      *
152600131016      * - Note (1 e 2) e Particolarità (3, 4, 5 e 6)
152700030515     c                   exsr      SchPARTIC
152800131016e   1 *
152900030716if  1c                   if           $NP(1) <> *blanks
153000030716     c                             or $NP(2) <> *blanks
153100030716     c                             or $NP(3) <> *blanks
153200030716     c                             or $NP(4) <> *blanks
153300030716     c                             or $NP(5) <> *blanks
153400030716     c                             or $NP(6) <> *blanks
153500030710     c                   eval      Pnp1   =  $NP(1)
153600030710     c                   eval      Pnp2   =  $NP(2)
153700030710     c                   eval      Pnp3   =  $NP(3)
153800030710     c                   eval      Pnp4   =  $NP(4)
153900030710     c                   eval      Pnp5   =  $NP(5)
154000030710     c                   eval      Pnp6   =  $NP(6)
154100030716e   1c                   endif
154200030515      *
154300080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
154400080401      *   (servizio consegna garantita x cliente Wuerth)
154500170413if  1c*//                if        DB0gva =  'P '
154600170413     c*//                movel     'PRIVATO'     Ppad
154700170413e   1c*//                endif
154800041012if  1c                   if        DB0gva =  'U '
154900041013     c                   eval      Ppad = 'CONSEGNA AD UFFICI'
155000041012e   1c                   endif
155100030515if  1c                   if           DB0tc1 =  'A'
155200030515     c                             or DB0tc2 =  'A'
155300030515     c                   move      'APPUNTAMENTO'Ppad
155400030515e   1c                   endif
155500080401if  1c                   if             DB0nrs = 40
155600090518     c                             and  DB0tsp = 'E'
155700080401     c                             and (DB0lnp = 043
155800080401     c                              or  DB0lnp = 006
155900080401     c                              or  DB0lnp = 063)
156000090304     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
156100090304     c                                     WÜRTH'
156200080401e   1c                   endif
156300030515      *
156400030515      * - Data e ora consegna richiesta
156500030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
156600091116      *   (NO "Prima del" per il cliente TTL)
156700091116if  0c                   if        DB0ccm <> c_CliTTL  or
156800091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
156900030515if  1c                   if        DB0dcr <> *zeros
157000030515     c                   eval      Past   =  '*'
157100030515     c     *iso          movel     DB0dcr        Data_EUR
157200030515     c     *eur          movel     Data_EUR      Pdcr
157300030515e   1c                   endif
157400030515if  1c                   if        DB0hcr <> *zeros
157500030515     c                   eval      Past   =  '*'
157600080430     c                   eval      Phcr   =  %editw(DB0hcr : '0 :  ')
157700030515e   1c                   endif
157800091116e   0c                   endif
157900030515      *
158000030515      * - Consegne particolari
158100030515     c                   eval      Pcp1   =  DB0dc1
158200030515     c                   eval      Pcp2   =  DB0dc2
158300030515      *
158400030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
158500030515      *   (importi "arrotondati" a 2 decimali in stampa)
158600030515if  1c                   if        DB0ift <> *zeros
158700080515     c                   eval      Pkim   = 'Imponibile'
158800080430     c                   eval(h)   w0112  =  DB0imv
158900080430     c                   eval      Pimp   =  %editc(w0112 : 'K')
159000030515sel 2c                   select
159100030515w   2c                   when      DB0iva <> *zeros
159200080515     c                   eval      Pkie   = 'I.v.a.    '
159300030624     c                   eval(h)   w0112  =  DB0iva
159400080430     c                   eval      Piva   =  %editc(w0112 : 'K')
159500030515w   2c                   when         DB0cei <> *blanks
159600030515     c                             or DB0dce <> *blanks
159700130213     c                   eval      Pkie   = 'Esenzione '
159800130213     c                   eval      Pese   = '           ' +
159900130213     c                                      %subst(DB0dce : 1 : %len(§EIdes))
160000030515e   2c                   endsl
160100080515     c                   eval      Pktf   = 'Tot. Fatt.'
160200080430     c                   eval(h)   w0112  =  DB0ift
160300080430     c                   eval      Pift   =  %editc(w0112 : 'K')
160400030515if  2c                   if        DB0div <> §GEDcn
160500030515     c                   eval      Pdiv   =  DB0div
160600030515e   2c                   endif
160700030515e   1c                   endif
160800030515      *
160900030515      * - Contrassegno e relativa divisa
161000030515if  1c                   IF        DB0cas <> *zeros
161100030625      *
161200080430     c                   eval(h)   w0112  =  DB0cas
161300080430     c                   eval      Pcas   =  %editc(w0112 : 'K')
161400030515if  2c                   if        DB0vca <> §GEDcn
161500030515     c                   eval      Pdca   =  DB0vca
161600030515e   2c                   endif
161700030515     c                   exsr      CtrCAs
161800030624      * - - Nuovo controllo abilitazione c/assegno
161900030624     c                   reset                   TRUL21DS
162000030624     c                   eval      I21cbo =  DB0cbo
162100030624     c                   eval      I21tsp =  DB0tsp
162200030624     c                   eval      I21lnp =  DB0lnp
162300030624     c                   eval      I21nzm =  DB0nzm
162400030624     c                   eval      I21lna =  DB0lna
162500030624     c                   eval      I21nzd =  DB0nzd
162600130826     c                   if        DB0ccm >  *zero
162700130826     c                   eval      I21ksc =  DB0ccm
162800130826     c                   else
162900130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
163000130826     c                   endif
163100030624     c                   eval      I21tic =  DB0tic
163200030624if  2c                   if        DB0vca <> §GEDcn
163300030624     c                   eval      I21imp =  Wcas
163400030624     c                   eval      I21div =  §GEDcn
163500030624x   2c                   else
163600030624     c                   eval      I21imp =  DB0cas
163700030624     c                   eval      I21div =  DB0vca
163800030624e   2c                   endif
163900080430     c                   eval      I21ute =  SDSusr
164000040607     c                   eval      I21pgm =  SDSpgm
164100030624     c                   call      'TRUL21R'
164200030624     c                   parm                    TRUL21ds
164300030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
164400030624     c                   exsr      Tab_1A
164500030624     c                   movel     §1Ades        DB0dti
164600030624if  2c                   if            §1Adeo <> *blanks
164700030624     c                             and O21fca <> *blanks
164800030624     c                   movel     §1Adeo        DB0dti
164900030624e   2c                   endif
165000030624      * - - Imposto i campi di stampa
165100080430     c                   eval      Pcti   =  DB0tic
165200080430     c                   eval      Ptic   =  DB0dti
165300030625      *
165400030515e   1c                   ENDIF
165500030515      *
165600030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
165700030515if  1c                   IF        DB0ift >  *zeros
165800030625      *
165900030515if  2c                   if            DB0vca <> §GEDcn
166000030515     c                             and DB0vca <> *blanks
166100030515     c                   eval      Pdie   =  DB0div
166200030515e   2c                   endif
166300030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
166400030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
166500030515      *   (importi "arrotondati" a 2 decimali in stampa)
166600030515sel 2c                   Select
166700110204      *   Non ho C/Assegno
166800101213w   2c                   When      DB0cas  = *zeros
166900030624     c                   eval(h)   w0112  =  DB0ift
167000030624     c                   eval      Pies   =  %editc(w0112 : 'K')
167100030625     c                   clear                   Pie1
167200030625     c                   clear                   Pdi1
167300030515      *   C/Assegno sommabile
167400030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
167500030515sel 3c                   select
167600030515      *   - c/assegno in EURO
167700030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
167800030624     c                   eval(h)   w0112  =  DB0ift + DB0cas
167900030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168000030624     c                   clear                   Pie1
168100030624     c                   clear                   Pdi1
168200030515      *   - c/assegno in DIVISA
168300030515w   3c                   when          DB0div =  §GEDcn
168400030515     c                             and DB0vca <> §GEDcn
168500030624     c                   eval(h)   w0112  =  DB0ift + Wcas
168600030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168700030624     c                   clear                   Pie1
168800030624     c                   clear                   Pdi1
168900030515e   3c                   endsl
169000100218      *   C/Assegno non sommabile
169100030515x   2c                   Other
169200030624     c                   eval(h)   w0112  =  DB0cas
169300030624     c                   eval      Pie1   =  %editc(w0112 : 'K')
169400100218     c*//                eval      Pdi1   =  DB0vca
169500030624     c                   eval(h)   w0112  =  DB0ift
169600030624     c                   eval      Pies   =  %editc(w0112 : 'K')
169700100218     c*//                eval      Pdie   =  DB0div
169800030515e   2c                   Endsl
169900030709      *
170000030709e   1c                   ENDIF
170100030515      *
170200030515      * - Numero e data fattura
170300030515if  1c                   if        DB0nft <> *zeros
170400080430     c                   eval      Pkft   =  'Fatt.'
170500080430     c                   eval      Pnft   =  %editc(DB0nft : 'Z')
170600130205     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
170700130205     c                                      ')'
170800030515if  2c                   if        DB0dft >  *zeros
170900080430     c                   eval      Pkdf   =  ' del '
171000030515     c     *iso          movel     DB0dft        Data_EUR
171100130213     c     *dmy          movel     Data_EUR      Pdft
171200030515e   2c                   endif
171300030515e   1c                   endif
171400030515      *
171500030515      * Memorizzo il codice tariffa come numero di copie da stampare
171600030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
171700030515if  1c                   if        DB0ris = 'M'  and  *in70
171800030515     c                   eval      Wctr   =  DB0ctr
171900030515e   1c                   endif
172000030515      *
172100030515     c                   ENDSR
172200170517      *
172300170517      /free
172400170517       //--------------------------------------------------------------
172500170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
172600170517       //?spedizione DPD.                                              ?
172700170517       //--------------------------------------------------------------
172800170517       BEGSR  sr_Mitt_DPD;
172900170517
173000170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
173100170517         clear  dCLI;
173200170517         reset  TIBS02ds;
173300170517         T02cod = 'CLI';
173400170517         T02ke1 = %editc( DB0ccm : 'X' );
173500170517         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
173600170517         if  T02err = *blank;
173700170517           dCLI = T02uni;
173800170517         endif;
173900170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
174000170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
174100170517           leavesr;
174200170517         endif;
174300170517
174400170517         // -?Forzatura Mittente da note "AM"?
174500170517         If  §CLItfntc = 'S';
174600170517
174700170517           if  NOT %open( TFNTC01L );
174800170517             open  TFNTC01L;
174900170517           endif;
175000170517           clear  keyTFNTC01;
175100170517           k_NTCapl = 'C';
175200170517           k_NTCnk1 = %editc( c_Kci : 'X' ) + %editc( DB0ccm : 'X' );
175300170517           k_NTCtnt = 'AM';
175400170517           chain  %kds( keyTFNTC01 )  TFNTC;
175500170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
175600170517             DB0rsm = NTCrnt;
175700170517           endif;
175800170517
175900170517         EndIf;
176000170517
176100170517         // -?Pulizia Indirizzo Mittente?
176200170517         If  §CLInoIMtt = 'S';
176300170517
176400170517           clear  DB0inm;
176500170517           clear  DB0cam;
176600170517           clear  DB0lom;
176700170517           clear  DB0prm;
176800170517           clear  DB0nzm;
176900170517
177000170517         EndIf;
177100170517
177200170517       ENDSR;
177300170517
177400170517      /end-free
177500030515      *
177600030515      *---------------------------------------------------------------*
177700050217      *?Inschieramento delle Note e delle Particolarità              ?*
177800030515      *---------------------------------------------------------------*
177900030515     c     SchPARTIC     BEGSR
178000030515      *
178100130924     c***?(già fatto:)?  reset                   $NoDesPartic
178200130924     c***?(già fatto:)?  clear                   $NP
178300130924     c***?(già fatto:)?  clear                   W_CoOr
178400030515     c                   clear                   XX
178500030710      *
178600030710      * Imposto eventuali note per prime
178700030710if  1c                   if        DB0nt1 <> *blanks
178800030710     c                   add       1             xx
178900030710     c                   movel     DB0nt1        $NP(xx)
179000030710e   1c                   endif
179100030710if  1c                   if        DB0nt2 <> *blanks
179200030710     c                   add       1             xx
179300030710     c                   movel     DB0nt2        $NP(xx)
179400030710e   1c                   endif
179500030515      *
179600030515      * Le particolarità possono essere lunghe fino a 45 char.
179700030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
179800030515      *   bastare, il resto lo si mette nella riga successiva.
179900030710      *   Se, invece, non risultano decodificate, si mettono solo i
180000030710      *   codici sulla stessa riga.
180100030710      *   Se uno solo risulta decodificato, esso va su una riga a
180200030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
180300030710      *                                    *on  = ok riga procedente)
180400030515      *
180500030515      * . . Particolarità Giacenza
180600030515if  1c                   IF        DB0gga <> *blanks
180700030515     c                   movel     'Q'           WTpPar
180800030515     c                   movel     DB0gga        WCdPar
180900030515     c                   exsr      TABxPAR
181000030710if  2c                   if            TBLuni       = *blanks
181100030710     c                             and $NoDesPartic = *on
181200030710     c                   eval      $NP(xx) = %trim($NP(xx))
181300030710     c                                     + '   ' + WCdPar
181400030710x   2c                   else
181500030710if  3c                   if        TBLuni       = *blanks
181600030710     c                   eval      $NoDesPartic = *on
181700030710x   3c                   else
181800030710     c                   eval      $NoDesPartic = *off
181900030710e   3c                   endif
182000030515     c                   add       1             xx
182100100517     c                   movel     w053a         $NP(xx)
182200100517if  3c                   if        %subst(w053a : %len($NP(xx))+1)
182300030515     c                              <> *blanks
182400030515     c                   add       1             xx
182500030515     c                   eval      $NP(xx) = '   ' +
182600100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
182700030710e   3c                   endif
182800030515e   2c                   endif
182900030515e   1c                   ENDIF
183000030515      *
183100030515      * . . Particolarità Consegna
183200030515if  1c                   IF        DB0gma <> *blanks
183300030515     c                   movel     'R'           WTpPar
183400030515     c                   movel     DB0gma        WCdPar
183500030515     c                   exsr      TABxPAR
183600030710if  2c                   if            TBLuni       = *blanks
183700030710     c                             and $NoDesPartic = *on
183800030710     c                   eval      $NP(xx) = %trim($NP(xx))
183900030710     c                                     + '   ' + WCdPar
184000030710x   2c                   else
184100030710if  3c                   if        TBLuni       = *blanks
184200030710     c                   eval      $NoDesPartic = *on
184300030710x   3c                   else
184400030710     c                   eval      $NoDesPartic = *off
184500030710e   3c                   endif
184600030515     c                   add       1             xx
184700100517     c                   movel     w053a         $NP(xx)
184800100517if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
184900030515     c                              <> *blanks
185000030515     c                   add       1             xx
185100030515     c                   eval      $NP(xx) = '   ' +
185200100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
185300030710e   3c                   endif
185400030515e   2c                   endif
185500030515e   1c                   ENDIF
185600030515      *
185700030515      * . . Particolarità Varie
185800030717      *     - questa descrizione va messa nell'apposito campo PPAD
185900041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
186000030717      *     - se 'B' (bancali da rendere al mittente) comparirà un
186100030717      *       apposito box
186200030717      *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
186300030717      *       comparirà un apposito box, un'apposita nota e l'indica-
186400030717      *       zione nella copia per il destinatario.
186500030717      *     ...Potrebbero anche non esserci più elementi di schiera
186600030717      *     disponibili [xx >= %elem($NP)] !
186700030709if  1c                   IF            DB0gva <> *blanks
186800030717     c                             and DB0gva <> 'B '
186900030717     c                             and DB0gva <> 'O '
187000030709     c                             and DB0gva <> 'P '
187100041015     c                             and DB0gva <> 'U '
187200030515     c                   movel     'S'           WTpPar
187300030515     c                   movel     DB0gva        WCdPar
187400030515     c                   exsr      TABxPAR
187500030710if  2c                   if            TBLuni       = *blanks
187600030710     c                             and $NoDesPartic = *on
187700030710     c                   eval      $NP(xx) = %trim($NP(xx))
187800030710     c                                     + '   ' + WCdPar
187900030710x   2c                   else
188000030710if  3c                   if        TBLuni       = *blanks
188100030710     c                   eval      $NoDesPartic = *on
188200030710x   3c                   else
188300030710     c                   eval      $NoDesPartic = *off
188400030710e   3c                   endif
188500030717if  3c                   if            xx < %elem($NP)
188600030709     c                   add       1             xx
188700100517     c                   movel     w053a         $NP(xx)
188800100517if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
188900030709     c                              <> *blanks
189000030717     c                             and xx < %elem($NP)
189100030709     c                   add       1             xx
189200030709     c                   eval      $NP(xx) = '   ' +
189300100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
189400030717e   4c                   endif
189500030717e   3c                   endif
189600030709e   2c                   endif
189700030515e   1c                   ENDIF
189800030717      *
189900030717      * Imposto il numero originale di colli componenti la spedizione
190000030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
190100030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
190200030717     c                   exsr      RepAR5bnb
190300030717if  2c                   if        W_CoOr  <> *blanks
190400030717     c                             and xx  <  %elem($NP)
190500030717     c                   add       1             xx
190600030717     c                   eval      $NP(xx) =  W_CoOr
190700030717e   2c                   endif
190800030717e   1c                   endif
190900030515      *
191000030515     c                   ENDSR
191100070319      *
191200070319      *---------------------------------------------------------------*
191300070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
191400070319      *---------------------------------------------------------------*
191500070319     c     SchConsParz   BEGSR
191600070319      *
191700130924     c***?(già fatto:)?  reset                   $CP
191800130924     c***?(già fatto:)?  reset                   $CPdcm
191900070319     c                   clear                   XX
192000070319      *
192100070319if  2c                   if        NOT %open(FNART01L)
192200101213     c                   open(e)   FNART01L
192300101213      /free
192400101213             if  %error;
192500101213               exsr  sr_ErrOpenFile;
192600101213             endif;
192700101213      /end-free
192800070319e   2c                   endif
192900070319      *
193000070514if  2c                   if        $Mamma = *off
193100070319     c     KLDV          setll     FNART000
193200070319     c     KLDV          reade     FNART000
193300070514x   2c                   else
193400070514     c     KLBL          setll     FNART000
193500070514     c     KLBL          reade     FNART000
193600070514e   2c                   endif
193700070319      *
193800070319do  1c                   DOW       NOT  %eof(FNART01L)
193900070319      *
194000070319if  2c                   If             ARTatb = *blanks
194100070319     c                             and  ARTdcm > *zeros
194200070319      *
194300070319     c                   eval      *in31  = *off
194400070319     c                   eval      xx     = 1
194500070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
194600070319if  3c                   if        NOT  *in31
194700070319     c                   eval      xx     = 1
194800070319     c     *zeros        lookup    $CPdcm(xx)                             31
194900070319if  4c                   if        *in31
195000140304     c*//                eval      $CP(xx)     = *zeros
195100140304     c                   clear                   Elem_CP
195200140304     c                   movel     Elem_CP       $CP(xx)
195300070319e   4c                   endif
195400070319e   3c                   endif
195500070319if  3c                   if        *in31
195600070319     c                   eval      $CPdcm(xx)  = ARTdcm
195700070319     c                   movel     $CP(xx)       Elem_CP
195800070319     c                   eval      WCPdcm      = ARTdcm
195900070319     c                   eval      WCPncl      = WCPncl + 1
196000070319     c                   movel     Elem_CP       $CP(xx)
196100070319e   3c                   endif
196200070319      *
196300070319e   2c                   EndIf
196400070319      *
196500070514if  2c                   if        $Mamma = *off
196600070319     c     KLDV          reade     FNART000
196700070514x   2c                   else
196800070514     c     KLBL          reade     FNART000
196900070514e   2c                   endif
197000070319      *
197100070319e   1c                   ENDDO
197200070319      *
197300140514if  0c                   If        xx     > *zero
197400140513      * Reperimento nominativo firmatario   &
197500140514     c                   exsr      sr_Firmatario
197600070319      * Ordinamento schiera per data
197700070319if  1c                   if        xx     > 1
197800070319     c                   sorta                   $CP
197900070319e   1c                   endif
198000140514e   0c                   EndIf
198100070319      *
198200070319     c                   ENDSR
198300140303      *
198400140303      /free
198500140303
198600140303       //--------------------------------------------------------------
198700140303       //?Reperimento nome firmatario                                  ?
198800140303       //--------------------------------------------------------------
198900140303       BEGSR  sr_Firmatario;
199000140303
199100140303if  1    if  NOT  %open(FNEVB22L);
199200140303           open  FNEVB22L;
199300140303e   1    endif;
199400140303
199500140303         clear  keyFNEVB22;
199600140303         k_EVB2aas = DB0aas;
199700140303         k_EVB2lnp = DB0lnp;
199800140303         k_EVB2nrs = DB0nrs;
199900140303         k_EVB2nsp = DB0nsp;
200000140303         k_EVB2cev = 'P  ';
200100140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
200200140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
200300140303         DoW  Not  %eof(FNEVB22L);
200400140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
200500140304             yy = %lookup( EVB2dev : $CPdcm );
200600140304             if  yy > *zero;
200700140304               Elem_CP = $CP(yy);
200800140303               if  WCPocm  <= EVB2hev;
200900140303                 WCPocm  = EVB2hev;
201000140303                 WCPfir  = EVB2dati;
201100140304                 $CP(yy) = Elem_CP;
201200140303               endif;
201300140303             endif;
201400140303           endif;
201500140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
201600140303         EndDo;
201700140303
201800140303       ENDSR;
201900140303
202000140303      /end-free
202100031124      *
202200031124      *---------------------------------------------------------------*
202300050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
202400031124      *---------------------------------------------------------------*
202500031124     c     RepAR5gen     BEGSR
202600031124      *
202700031124     c                   clear                   dAR5gen
202800031124      *
202900031124     c                   eval      AR5trd = 'GEN'
203000031127     c     K05AR501      chain     FIAR5000
203100031124      *
203200101213if  1c                   if            %found(FIAR501L)
203300040301     c                             and AR5atb =  *blanks
203400031124     c                   movel     AR5uni        dAR5gen
203500031124e   1c                   endif
203600031124      *
203700031124     c                   ENDSR
203800030717      *
203900030717      *---------------------------------------------------------------*
204000050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
204100030717      *---------------------------------------------------------------*
204200030717     c     RepAR5bnb     BEGSR
204300030717      *
204400030717     c                   clear                   dAR5bnb
204500130924     c***?(già fatto:)?  clear                   W_CoOr
204600030717      *
204700030717     c                   eval      AR5trd = 'BNB'
204800030717     c     K05AR501      chain     FIAR5000
204900030717      *
205000101213if  1c                   if            %found(FIAR501L)
205100040301     c                             and AR5atb =  *blanks
205200030717     c                   movel     AR5uni        dAR5bnb
205300030717if  2c                   if        §AR5bcor <> *zeros
205400030717     c                   eval      W_CoOr   =  C_CoOr
205500030717     c                                      +  %editc(§AR5bcor : 'K')
205600030717e   2c                   endif
205700030717e   1c                   endif
205800030717      *
205900030717     c                   ENDSR
206000030515      *
206100030515      *---------------------------------------------------------------*
206200051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
206300030515      *---------------------------------------------------------------*
206400110208     c     sr_DecodLDV   BEGSR
206500030515      *
206600030515      * Richiesta firma per ricevuta della merce senza riserve
206700030515      * (e NON  quella  per quietanza) => *in35 + not *in36
206800030515     c                   movea     '10'          *in(35)
206900070307      *
207000070307      * Intestazione moduli
207100090619      *
207200090619      /free
207300121018
207400121018         // -?Indicazione della Partiva IVA o del Cod. Fiscale del Destinatario?
207500121018         //  ?se Porto Assegnato con Fattura immediata?
207600121018         //  ?- NON se destinatario estero -?
207700121018             // -?con fattura?
207800121018         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
207900121018             // - -?P.A. (con fattura immediata)?
208000121018             //    ?(con destinatario italiano)?
208100121018             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
208200121018             // - -?con 2ª bolla?
208300121018             //    ?(con destinatario italiano)?
208400131203              (DB0tb2 <> *blank         and  DB0nzd  = *blank) );
208500121018           select;
208600131010             // ·?Esiste Partita Iva NON FITTIZIA?
208700131010             when  wB0piv <> *blank  and
208800121026                   %scan('$$' : wB0piv) = *zero;
208900121018               PpivD = 'Partita Iva: ' + wB0piv;
209000131010             // ·?Esiste Codice Fiscale?
209100131010             when  wB0cfi <> *blank;
209200121018               PpivD = 'Codice Fiscale: ' + wB0cfi;
209300121018           endsl;
209400121018         ENDIF;
209500110204
209600110204         // -?Consegna al mattino (tempi consegna = 12 ore)?
209700110204         //  ?(NON per Würth)?
209800110204         If  §VPOrcm <= wDate and
209900090619             DB0tsp   = 'E'   and
210000090619            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
210100090619                                   DB0lnp <> 006   and
210200090619                                   DB0lnp <> 063));
210300090619           clear  TNSD99ds;
210400101213           D98tbo = 'A';
210500090619           D98aas = DB0aas;
210600090619           D98lnp = DB0lnp;
210700090619           D98nrs = DB0nrs;
210800090619           D98nsp = DB0nsp;
210900090619           tnsd99r ( TNSD99ds );
211000120910           if  D98tcc = 12;
211100090619             %subst(Pds2 : %len(Pds2) - 1) = ' M';
211200090619           endif;
211300110204         EndIf;
211400110204
211500110204         // -?"NON UTILIZZARE PER LA CONSEGNA"?
211600110204         //  ?(stampa della sola copia per il vettore)?
211700110204         if  $No_x_Cons = *on;
211800110204           *in35 = *off;
211900110204           //*in36 = *off;
212000110204           // -?costante da stampare se LDV recapito c/assegni stampata?
212100110204           //  ?dal pgm "stampa DDT NO in arrivo"?
212200110204           //Pnux   =  ' NON UTILIZZARE PER LA CONSEGNA';
212300110204         endif;
212400110204
212500110204         // -?Decodifica "Riserva Consegna" (se c'è)?
212600110204         clear  ds2A;
212700110204         If  ILVriserv <> *blank;
212800110204           TBLcod = '2A';
212900110204           TBLkey = ILVriserv;
213000110207           chain  K03TAB  TABEL;
213100110204           if  %found(TABEL00F);
213200110204             ds2A = TBLuni;
213300110204           endif;
213400110204         EndIf;
213500140514
213600140514         // -?Reperimento consegne parziali precedenti da FNART?
213700140514         If  $ConsParz;
213800140514           exsr  SchConsParz;
213900140514           if  xx < 1;
214000140514             $ConsParz = *off;
214100140514           endif;
214200140514         EndIF;
214300110204
214400110204         // -?Costante "PER RICEVUTA DELLA MERCE..."?
214500130716         exsr  sr_PerRicevutaDellaMerce;
214600110204
214700090619      /end-free
214800030515      *
214900130411      * Verifico se Packing-List e/o PDF
215000130411     c                   select
215100130411     c                   when      $§PDF  = *on
215200130411     c                   eval      Ppkl   =  'PACKING LIST PDF'
215300130411     c                   when      $FIAR8 =  *on
215400130411     c                   eval      Ppkl   =  'PACKING LIST    '
215500130411     c                   endsl
215600030515      *
215700030515      * Flag "*" > x Consegne particolari
215800030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
215900030515     c                   eval      Past   =  '*'
216000030515     c                   endif
216100030515      *
216200030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
216300030515if  1c                   IF        DB0cas <> *zeros
216400030515      * - Flag "*" > x C/Assegno
216500030904if  2c                   if        O21fca <> *blanks
216600030904     c                   movel     '*'           Past
216700030515e   2c                   endif
216800030624      * - Imposto i campi di stampa
216900080430     c                   eval      Pcti   =  DB0tic
217000080430     c                   eval      Ptic   =  DB0dti
217100030515e   1c                   ENDIF
217200030515      *
217300040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
217400030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
217500030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
217600030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
217700030515     c                             or        DB0tb2     <> *blanks
217800030515     c                   eval      Pass   =  'A'
217900030515if  2c                   if        DB0nft =  *zeros
218000080430     c                   if        Pkie   =  *blanks
218100080430     c                   evalr     Pkie   =  'SEGUE'
218200080430     c                   eval      Piva   =  ' FATTURA'
218300080430     c                   else
218400080430     c                   eval      Piva   =  'SEGUE FATTURA'
218500080430     c                   endif
218600030515e   2c                   endif
218700030515e   1c                   endif
218800030515      *
218900030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
219000051129if  1c                   IF            DB0dcr >  *zeros
219100051129     c                             or  DB0hcr >  *zeros
219200051129     c                   clear                   FNLV80ds
219300051129     c                   eval      ILV80aas   =  DB0aas
219400051129     c                   eval      ILV80lnp   =  DB0lnp
219500051129     c                   eval      ILV80nrs   =  DB0nrs
219600051129     c                   eval      ILV80nsp   =  DB0nsp
219700051129     c                   eval      ILV80mgs   =  DB0mgs
219800051129     c                   eval      ILV80tc1   =  DB0tc1
219900051129     c                   eval      ILV80tc2   =  DB0tc2
220000051129     c                   eval      ILV80dcr   =  DB0dcr
220100051129     c                   eval      ILV80hcr   =  DB0hcr
220200051129     c                   eval      ILV80tcr   =  DB0tcr
220300051129     c                   call      'FNLV80R'
220400051129     c                   parm                    KPJBA
220500051129     c                   parm                    FNLV80ds
220600051129sel 2c                   Select
220700051129      * errore
220800051129w   2c                   When      OLV80err  <>  *blanks
220900051129      * consegna tassativa (per ora solo ai supermercati)
221000051129w   2c                   When      OLV80tat   =  'S'
221100081021     c                   movel     OLV80tcrd     Ptdr
221200091116      * NO "Prima del" per il cliente TTL
221300091116w   2c                   When      DB0ccm     = c_CliTTL   and
221400091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
221500051129      * prima/dopo il/delle
221600051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
221700110208      *  nella subr. "sr_TipoLDV")
221800081022x   2c                   Other
221900081021     c                   movel     OLV80tcrd     Ptdr
222000081021e   2c                   Endsl
222100051129e   1c                   ENDIF
222200030515      *
222300030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
222400060921      * (SE?non?è "Fermo Deposito")
222500030515      * 1° turno di chiusura
222600060921if  1c                   IF             DB0gc1 <> *blanks
222700060921     c                             and  DB0ffd  = *blanks
222800030515      * - Flag "*" > Turno di chiusura
222900030515     c                   eval      Past   =  '*'
223000030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
223100030515      * - Nessun giorno specificato - Solo 'M' o 'P'
223200130924     c***?già così:?     clear                   Pgc1
223300030515if  3c                   if        DB0gc1 =  ' M'
223400030515     c                   eval      Pgc1   =  %trim($Gio(8))
223500030515x   3c                   else
223600030515     c                   eval      Pgc1   =  %trim($Gio(9))
223700030515e   3c                   endif
223800030515x   2c                   Else
223900030515      * - Giorno specificato
224000030515     c                   movel     DB0gc1        w001a
224100030515     c                   eval      XX     =  1
224200030515     c                   eval      *in31  =  *off
224300030515     c     w001a         lookup    $GgS(xx)                               31
224400030515     c   31              movel     $Gio(xx)      Pgc1
224500030515      *   + 'M' o 'P'
224600030515sel 3c                   select
224700030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
224800030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
224900030515     c                                    +  %trim($Gio(8))
225000030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
225100030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
225200030515     c                                    +  %trim($Gio(9))
225300030515e   3c                   endsl
225400030515e   2c                   Endif
225500030515e   1c                   ENDIF
225600030515      * 2° turno di chiusura
225700060921if  1c                   IF             DB0gc2 <> *blanks
225800060921     c                             and  DB0ffd  = *blanks
225900030515      * - Flag "*" > Turno di chiusura
226000030515     c                   eval      Past   =  '*'
226100030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
226200030515      * - Nessun giorno specificato - Solo 'M' o 'P'
226300130924     c***?già così:?     clear                   Pgc2
226400030515if  3c                   if        DB0gc2 =  ' M'
226500080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
226600080515     c                                                = %trim($Gio(8))
226700030515x   3c                   else
226800080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
226900080515     c                                                = %trim($Gio(9))
227000030515e   3c                   endif
227100030515x   2c                   Else
227200030515      * - Giorno specificato
227300030515     c                   movel     DB0gc2        w001a
227400030515     c                   eval      XX     =  1
227500030515     c                   eval      *in31  =  *off
227600030515     c     w001a         lookup    $GgS(xx)                               31
227700080515     c   31              eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
227800080515     c                                                = %trim($Gio(xx))
227900030515      *   + 'M' o 'P'
228000030515sel 3c                   select
228100030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
228200080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
228300030515     c                                    +  %trim($Gio(8))
228400030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
228500080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
228600030515     c                                    +  %trim($Gio(9))
228700030515e   3c                   endsl
228800030515e   2c                   Endif
228900030515e   1c                   ENDIF
229000080523      /free
229100080523       // ________________________________________
229200080523       // Impostazione nominativo del destinatario
229300080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
229400110214         exsr  RepAR4_1;
229500080523       // _______________________________________
229600080523       // Impostazione data ed ora della consegna
229700080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
229800110228         //?ATTENZIONE:?il "RESET" di un campo di tipo "d" (data)?
229900110228         //?riporta in tale campo la *date, NON quella impostata?
230000110228         //?come "inz(...)" della specifica "d" !!!!!!!!!!!!!!!!?
230100110228         Data_EUR = *loval;
230200110209         clear  w0080;
230300110209         select;
230400110209           when  wB0dcm > *zero;
230500110209             Data_EUR = %date( wB0dcm : *iso );
230600110209           when  wB0dcp > *zero;
230700110209             Data_EUR = %date( wB0dcp : *iso );
230800110209         endsl;
230900110209         if  Data_EUR > *loval;
231000110209           w0080 = ( ( %subdt(Data_EUR : *days) * 1000000 ) +
231100110209                     ( %subdt(Data_EUR : *months) * 10000 ) +
231200110209                     ( %subdt(Data_EUR : *years) ) );
231300110209         endif;
231400150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
231500081103           Pfedkcd  = %editc( w0080 : 'Y' );
231600120702           if  dILVlibero.§ILVhmp > *zero;
231700081103             Pfedkcd = %trimr( Pfedkcd ) + '  '
231800120702                     + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
231900120702                     + %subst( dILVlibero.§ILVhmp : 3 : 2 );
232000081103           endif;
232100081103         else;
232200081103           Pkcd  = %editc( w0080 : 'Y' );
232300120702           if  dILVlibero.§ILVhmp > *zero;
232400081103             Pkcd = %trimr( Pkcd ) + '   '
232500120702                  + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
232600120702                  + %subst( dILVlibero.§ILVhmp : 3 : 2 );
232700081103           endif;
232800081103         endif;
232900080523      /end-free
233000071114      *
233100071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
233200030515      *
233300071114      * Se pgm richiamato x stampa LdV in arrivo:
233400030515if  1c                   IF        DB0fl1 =  'A'
233500071114      * - imposto l'"*" in base al parametro ricevuto.
233600030515if  2c                   if        DB0fl2 =  '*'
233700030515     c                   eval      Past   =  '*'
233800030515x   2c                   else
233900030515     c                   clear                   Past
234000030515e   2c                   endif
234100071114      * - idem per flag di bolla non abilitata
234200030515     c                   eval      Pdis   =  DB0fl5
234300030515e   1c                   endif
234400030515      *
234500030515     c                   ENDSR
234600080430      *
234700080430      /free
234800130716
234900130716       //--------------------------------------------------------------
235000130716       //?Impostazione "PER RICEVUTA DELLA MERCE ..."                  ?
235100130716       //--------------------------------------------------------------
235200130716       BEGSR  sr_PerRicevutaDellaMerce;
235300130716
235400130716         Select;
235500130716
235600130716           // -?danon?stampare se?
235700130716           // ·?non utilizzare per la consegna  (NOT *in35)?
235800130716           When  NOT *in35;
235900130716             //clear  Pkrm;       ?(già fatto!)?
236000140108             //clear  Pkrm2;      ?(già fatto!)?
236100130716
236200130716           // -?costante per "DPD"   (come standard)?
236300130716           //         ?o per "FedEx" (come standard)?
236400130716           When  *in71  or  *in72;
236500130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
236600130716             // -?senza riserve?
236700130716             if  ILVriserv = *blank;
236800130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
236900130716             // -?con Riserva Consegna?
237000130716             else;
237100130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
237200130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
237300130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
237400130716               endif;
237500130716             endif;
237600130716
237700130716           // -?costante per cliente?
237800130716           //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
237900140513           When  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
238000130716             exsr  sr_RepDdtCLN;
238100130716             Pkrm = 'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
238200130716                     DDT '
238300130716                  + %trimr( %editc (WddtCLN : 'Z') )
238400130716                  + ' DEL '
238500130716                  + %editc (WdspCLN : 'Y')
238600130716                  + ' CON RISERVA DI CONTROLLO COME CONTRATTO';
238700130716             if  Wdsp < 20101001;
238800130716               Pkrm = %trimr(Pkrm) + ' C.L.N.';
238900130716             else;
239000130716               Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
239100130716             endif;
239200130716
239300130716           // -?costante "standard" se "DDT sì"?
239400130716           When  §VPOrsr <= wDate  and  DB0bam = 'S';
239500130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
239600130716                     TRASPORTO';
239700130716             // -?senza riserve?
239800130716             if  ILVriserv = *blank;
239900130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
240000130716             // -?con Riserva Consegna?
240100130716             else;
240200130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
240300130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
240400130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
240500130716               endif;
240600130716             endif;
240700130716
240800130716           // -?costante "standard" se "DDT no" + Packing-List?
240900161012           When  DB0bam = *blank  and  ($FIAR8 = *on  or  $§PDF = *on);
241000130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
241100130716                     TRASPORTO';
241200130716             // -?senza riserve?
241300130716             if  ILVriserv = *blank;
241400130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
241500130716             // -?con Riserva Consegna?
241600130716             else;
241700130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
241800130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
241900130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
242000130716               endif;
242100130716             endif;
242200130716
242300130716           // -?costante "standard" altrimenti?
242400130716           Other;
242500130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
242600130716             // -?senza riserve?
242700130716             if  ILVriserv = *blank;
242800130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
242900130716             // -?con Riserva Consegna?
243000130716             else;
243100130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
243200130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
243300130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
243400130716               endif;
243500130716             endif;
243600130716
243700130716         EndSl;
243800130716
243900130716         // -?costante aggiuntiva per cliente?
244000130716         //  ?"S G Soluzioni Logistiche"?
244100130716         If  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
244200130716
244300130716           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
244400130716             Pkrm2 = 'INCLUDE CONTRATTO - '
244500130716                   + %trim(DB0nas);
244600130716           endif;
244700130716           if  DB0rmo <> *blank;
244800130716             if  Pkrm2 <> *blank;
244900130716               Pkrm2 = %trimr(Pkrm2)
245000130716                     + ' - MATR TV: '
245100130716                     + %trim(DB0rmo);
245200130716             else;
245300130716               Pkrm2 = 'MATR TV: '
245400130716                     + %trim(DB0rmo);
245500130716             endif;
245600130716           endif;
245700130716
245800130716         EndIf;
245900140108
246000140108         // -?Consegne Parziali Precedenti?
246100140108         If  $ConsParz = *on  and  wB0dcm = *zero;
246200140108
246300140108           Select;
246400140110             When  Pkrm2 = *blank;
246500140110               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
246600140108             When  %subst( Pkrm : 111 ) = *blank;
246700140108               Pkrm = %subst( Pkrm : 1 : 110 ) + ' PARZIALE';
246800140108             When  %subst( Pkrm2 : 111 ) = *blank;
246900140108               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
247000140108           EndSl;
247100140108
247200140108         EndIf;
247300130716
247400130716       ENDSR;
247500090619
247600090619       //--------------------------------------------------------------
247700130716       //?Reperimento Numero e Data del DDT originale del mittente     ?
247800130716       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
247900090619       //--------------------------------------------------------------
248000090619       BEGSR  sr_RepDdtCLN;
248100090619
248200130716         // -?Verifica se la spedizione ha mamma?
248300090619         if  not %open(FNLBL01L);
248400101213           open(e)  FNLBL01L;
248500101213           if  %error;
248600101213             exsr  sr_ErrOpenFile;
248700101213           endif;
248800090619         endif;
248900090619         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
249000090619
249100090619         Select;
249200090619
249300130716           // -?Con Mamma: reperimento dati DDT dalla mamma?
249400090619           When  %found(FNLBL01L);
249500090619             if  not %open(FNBLP01L);
249600101213               open(e)  FNBLP01L;
249700101213               if  %error;
249800101213                 exsr  sr_ErrOpenFile;
249900101213               endif;
250000090619             endif;
250100090619             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
250200090619             if  %found(FNBLP01L);
250300090619               exsr  sr_DatiCLN_blp;
250400090619             endif;
250500090619
250600130716           // -?In Arrivo: reperimento dati DDT da FNARB?
250700090619           Other;
250800090619             exsr  sr_DatiCLN_arb;
250900090619
251000090619         EndSl;
251100090619
251200090619       ENDSR;
251300090619
251400090619       //--------------------------------------------------------------
251500090619       //?In Partenza: reperimento dati DDT da FNBLP                   ?
251600090619       //--------------------------------------------------------------
251700090619       BEGSR  sr_DatiCLN_blp;
251800090619
251900130716         // -?Numero DDT?
252000090619         select;
252100090619           when  BLPrmn <> *zero;
252200100906             if  BLPrmn  > -99999999   and
252300100906                 BLPrmn <   99999999;
252400100906               WddtCLN = BLPrmn;
252500100906             endif;
252600090619           when  BLPncd <> *zero;
252700090619             WddtCLN = BLPncd;
252800090619         endsl;
252900090619
253000130716         // -?Data DDT in formato gg/mm/aaaa?
253100090619         Data_Dmy = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
253200100906         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
253300100906                      ( %subdt(Data_Dmy : *months) * 100 ) +
253400100906                ( %int( %subst( %editc(BLPaas : 'X') : 3 : 2 ) ) ) );
253500090619
253600090619       ENDSR;
253700090619
253800090619       //--------------------------------------------------------------
253900090619       //?In Arrivo: reperimento dati DDT da FNARB                     ?
254000090619       //--------------------------------------------------------------
254100090619       BEGSR  sr_DatiCLN_arb;
254200090619
254300130716         // -?Numero DDT?
254400090619         select;
254500090619           when  ARBrmn <> *zero;
254600100906             if  ARBrmn  > -99999999   and
254700100906                 ARBrmn <   99999999;
254800100906               WddtCLN = ARBrmn;
254900100906             endif;
255000090619           when  ARBncd <> *zero;
255100090619             WddtCLN = ARBncd;
255200090619         endsl;
255300090619
255400130716         // -?Data DDT in formato gg/mm/aaaa?
255500090619         Data_Dmy = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
255600090619         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
255700090619                      ( %subdt(Data_Dmy : *months) * 100 ) +
255800110117                ( %int( %subst( %editc(ARBaas : 'X') : 3 : 2 ) ) ) );
255900090619
256000090619       ENDSR;
256100100517
256200100517       //--------------------------------------------------------------
256300100517       //?Verifica se presente ORM con preavviso mail                  ?
256400100517       //--------------------------------------------------------------
256500110117       BEGSR  sr_CheckORM_PreavvMail;
256600100517
256700130924         //clear  dOrm01;             ?(già fatto)?
256800100517
256900130925         // -?Reperimento num. ORM?
257000130924         exsr  RepAR4_M;
257100100517
257200130925         // -?SE ORM inesistente: inutile proseguire?
257300130925         if  §B4poe = *zero   or   §B4nor = *zero;
257400100517           leavesr;
257500100517         endif;
257600100517
257700130716         // -?Verifica presenza O.R.M.?
257800100517         if  Not  %open(FNORM01L);
257900101213           open(e)  FNORM01L;
258000101213           if  %error;
258100101213             exsr  sr_ErrOpenFile;
258200101213           endif;
258300100517         endif;
258400100517
258500100517         ORMpoe = §B4poe;
258600100517         ORMnsr = §B4nsr;
258700100517         ORMnor = §B4nor;
258800100517         ORMnrv = §B4nrv;
258900100517
259000100517         chain  K04ORM01  FNORM000;
259100100517
259200130716         // -?ORM non reperito: inutile proseguire?
259300100517         if  Not  %found(FNORM01L);
259400100517           leavesr;
259500100517         endif;
259600100517
259700130716         // -?Impostazione flag per stampa porto "ASSEGNATO"?
259800100517         dOrm01 = ORMflo;
259900100517         $PreavvORM = (§ORMpre = 'M');
260000100517
260100100517       ENDSR;
260200090619
260300080430       //--------------------------------------------------------------
260400140303       //?Preparazione dati di output relativi alla Lettera di Vettura ?
260500080430       //--------------------------------------------------------------
260600131016       BEGSR  sr_DatiOUT;
260700080430
260800101221         // -?Impostazione righe?
260900080430         clear LdV;
261000080430         clear wx;
261100080430
261200101221         // -?Area immagine LdV?
261300080801
261400110203         FOR  wx = 1  TO  c_Row;
261500080430
261600080430           %occur(LdV) = wx;
261700080430
261800080508           LdvPrg      = wx;
261900080430           select;
262000080430
262100101221             // ·? 1ª riga?
262200110203             when  wx = 1;
262300110203             LdvTxt = ds_01;
262400080430
262500101221             // ·? 2ª riga (vuota)?
262600110203             when  wx = 2;
262700080430             clear LdvTxt;
262800080430
262900101221             // ·? 3ª riga?
263000110203             when  wx = 3;
263100110203             LdvTxt = ds_03;
263200080430
263300101221             // ·? 4ª riga?
263400110203             when  wx = 4;
263500110203             LdvTxt = ds_04;
263600080430
263700101221             // ·? 5ª riga?
263800110203             when  wx = 5;
263900110203             LdvTxt = ds_05;
264000080430
264100101221             // ·? 6ª riga (vuota)?
264200110203             when  wx = 6;
264300110203             clear LdvTxt;
264400080430
264500101221             // ·? 7ª riga?
264600110203             when  wx = 7;
264700110203             LdvTxt = ds_07;
264800080430
264900101221             // ·? 8ª riga (vuota)?
265000110203             when  wx = 8;
265100080430             clear LdvTxt;
265200080430
265300101221             // ·? 9ª riga?
265400110203             when  wx = 9;
265500110203             LdvTxt = ds_09;
265600080430
265700101221             // ·?10ª riga?
265800110203             when  wx = 10;
265900110203             LdvTxt = ds_10;
266000080430
266100101221             // ·?11ª riga?
266200110203             when  wx = 11;
266300110203             LdvTxt = ds_11;
266400080430
266500101221             // ·?12ª riga?
266600110203             when  wx = 12;
266700110203             LdvTxt = ds_12;
266800080430
266900101221             // ·?13ª riga?
267000110203             when  wx = 13;
267100110203             LdvTxt = ds_13;
267200080430
267300101221             // ·?14ª riga?
267400110203             when  wx = 14;
267500110203             LdvTxt = ds_14;
267600080430
267700101221             // ·?15ª riga?
267800110203             when  wx = 15;
267900110203             LdvTxt = ds_15;
268000080430
268100101221             // ·?16ª riga?
268200110203             when  wx = 16;
268300110203             LdvTxt = ds_16;
268400080430
268500101221             // ·?17ª riga?
268600110203             when  wx = 17;
268700110203             LdvTxt = ds_17;
268800080430
268900101221             // ·?18ª riga?
269000110203             when  wx = 18;
269100110203             LdvTxt = ds_18;
269200080430
269300101221             // ·?19ª riga?
269400110203             when  wx = 19;
269500110203             LdvTxt = ds_19;
269600080430
269700101221             // ·?20ª riga?
269800110203             when  wx = 20;
269900110203             LdvTxt = ds_20;
270000080430
270100101221             // ·?21ª riga?
270200110203             when  wx = 21;
270300110203             LdvTxt = ds_21;
270400080430
270500101221             // ·?22ª riga?
270600110203             when  wx = 22;
270700110203             LdvTxt = ds_22;
270800080430
270900101221             // ·?23ª riga?
271000110203             when  wx = 23;
271100110203             LdvTxt = ds_23;
271200080430
271300101221             // ·?24ª riga?
271400110203             when  wx = 24;
271500110203             LdvTxt = ds_24;
271600080430
271700101221             // ·?25ª riga?
271800110203             when  wx = 25;
271900110203             LdvTxt = ds_25;
272000080430
272100101221             // ·?26ª riga (vuota)?
272200110203             when  wx = 26;
272300080806             clear LdvTxt;
272400080430
272500101221             // ·?27ª riga?
272600110203             when  wx = 27;
272700110203             LdvTxt = ds_27;
272800080430
272900101221             // ·?28ª riga?
273000110203             when  wx = 28;
273100110203             LdvTxt = ds_28;
273200081021
273300101221             // ·?29ª riga?
273400110203             when  wx = 29;
273500081021             LdvTxt = ds_29;
273600081021
273700101221             // ·?30ª riga?
273800110203             when  wx = 30;
273900081021             LdvTxt = ds_30;
274000081021
274100110215             // ·?31ª riga?
274200110203             when  wx = 31;
274300110215             LdvTxt = ds_31;
274400081021
274500101221             // ·?32ª riga?
274600110203             when  wx = 32;
274700081021             LdvTxt = ds_32;
274800081021
274900101221             // ·?33ª riga?
275000110203             when  wx = 33;
275100081021             LdvTxt = ds_33;
275200081021
275300101221             // ·?34ª riga?
275400110203             when  wx = 34;
275500081021             LdvTxt = ds_34;
275600081021
275700101221             // ·?35ª riga (vuota)?
275800110203             when  wx = 35;
275900081021             clear LdvTxt;
276000081021
276100101221             // ·?36ª riga (vuota)?
276200110203             when  wx = 36;
276300081021             clear LdvTxt;
276400080430
276500080430           endsl;
276600080430
276700080430         ENDFOR;
276800080430
276900110518         // -?Impostazione iniziale del modulo standard:?
277000131203         //// ·?Modulo standard con logo Bartolini?
277100131203         //TemplateName = 'ldvbase00 ';
277200131203         //// ·?Modulo standard con logo Bartolini-BRT?
277300131203         //TemplateName = 'ldvbase10 ';
277400120524         // ·?Modulo standard con logo BRT?
277500120524         TemplateName = 'ldvbase20 ';
277600080801
277700110203         Select;
277800081030
277900101221           // -?Area dati "DPD"?
278000101221           //  ?(NON della LNP, ma dalla fil. ricavata dal cod.mittente)?
278100110203           When  §CMntw = 'DPD';
278200110203             exsr  sr_LdVbaseDPD;
278300081030
278400101221           // -?Area dati "FedEx"?
278500150430           When  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
278600110203             exsr  sr_LdVbaseFED;
278700081030
278800131206           // -?Area box (ex "Promemoria per l'autotrasportatore")?
278900131206           When  $1o_Passaggio = *on  or  $ConsParz = *on;
279000110203             exsr  sr_LdVbase01;
279100081030
279200110203         EndSl;
279300130411
279400130411
279500130411         // ? CHIODI PER TEST                               ?
279600130411         Select;
279700130411           // -?Nessun TEST?
279800130411           When  $Test = *blank;
279900130411           // -?Spedizione FedEx?
280000130411           When  $Test = 'FED';
280100130411             TemplateName = 'ldvbase23 ';
280200130411           // -?Spedizione DPD?
280300130411           When  $Test = 'DPD';
280400130411             TemplateName = 'ldvbase22 ';
280500130411           // -?Spedizione con Consegna Parziale Precedente?
280600130411           When  $Test = 'C_P';
280700130411             TemplateName = 'ldvbase21 ';
280800130411           // -?Spedizione Normale?
280900130411           When  $Test = 'NOR';
281000130411             TemplateName = 'ldvbase20 ';
281100130411         EndSl;
281200130411
281300080801
281400101221         // -?Vuoti finali?
281500130213         For  wx = wx  To  w_Row;
281600080801           %occur(LdV) = wx;
281700130213           if  LdVPrg = *zero;
281800130213             LdvPrg = wx;
281900130213             clear  LdvTxt;
282000130213           endif;
282100110203         EndFor;
282200130205
282300130205         // -?Dati aggiuntivi per la 14ª riga (esenzione Iva)?
282400130205         if  DB0ift <> *zero  and
282500130205             DB0iva =  *zero  and
282600130205            (DB0cei <> *blank  or  DB0dce <> *blank);
282700130213           w_Row += 1;
282800130213           wx = w_Row;
282900130205           %occur(LdV) = wx;
283000130205           LdvPrg = 140;
283100130205           LdvTxt = ds_140;
283200130205         endif;
283300130205         // -?Dati aggiuntivi per la 18ª riga (n° fatt. e fil. Iva)?
283400130205         if  DB0nft <> *zero;
283500130213           w_Row += 1;
283600130213           wx = w_Row;
283700130205           %occur(LdV) = wx;
283800130205           LdvPrg = 180;
283900130205           LdvTxt = ds_180;
284000130205         endif;
284100110201
284200110201         // -?Codice a barre: ANNO+LNP+NRS+NSP+CKD:?
284300110201         //  ?richiama il pgm TRUL28R per ricavarne il check digit?
284400110201         reset  TRUL28ds;
284500110201         I28cod = BarCodeDS;
284600110201         TRUL28R1 ( TRUL28ds );
284700110201
284800110201         // -?Impostazione campo "LastRow"?
284900110201         LastRow = 'Spedizione: ' + %subst( O28cod : 1 : 15 ) + '  ' +
285000110203                   'Distinta: ' + %subst( ILVprmDst : 1 : 3 ) + '-'
285100120419                                + %subst( ILVprmDst : 4 );
285200080801
285300110203         // ______________________________
285400110203         //?Stampa di prova (SE richiesta)?
285500110203         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
285600110519         If  SDSprm > 11;
285700130716           exsr  sr_Print;
285800130716         EndIf;
285900080430
286000080430       ENDSR;
286100130716
286200130716       //--------------------------------------------------------------
286300130716       //?Stampa dei dati per l'immagine - SE RICHIAMATO PER PROVA     ?
286400130716       //--------------------------------------------------------------
286500130716       BEGSR  sr_Print;
286600130716
286700130716         // -?Apertura file di stampa?
286800130716         if not %open(QSYSPRT);
286900130716           open  QSYSPRT;
287000130716         endif;
287100130716
287200130716         // -?Stampa "testata"?
287300130716         except PRTtxt;
287400130716
287500130716         // -?Stampa dettaglio?
287600130716         for wx = 1 to w_Row;
287700130716           %occur(LdV) = wx;
287800130716           except PRTrow;
287900130716         endfor;
288000130716
288100130716         // -?Stampa "ultima riga"?
288200130716         except PRTend;
288300130716
288400130716         // -?Chiusura file di stampa?
288500130716         close QSYSPRT;
288600130716
288700130716       ENDSR;
288800081030
288900081030       //--------------------------------------------------------------
289000130716       //?Preparazione dati x spedizione "DPD"                         ?
289100081030       //--------------------------------------------------------------
289200130716       BEGSR  sr_LdVbaseDPD;
289300081030
289400120524         // -?Impostazione del modulo DPD:?
289500131203         //// ·?Modulo DPD con logo Bartolini?
289600131203         //TemplateName = 'ldvbase02 ';
289700131203         //// ·?Modulo DPD con logo Bartolini/BRT?
289800131203         //TemplateName = 'ldvbase12 ';
289900120524         // ·?Modulo DPD con logo BRT?
290000120524         TemplateName = 'ldvbase22 ';
290100081030
290200101221         // -?Impostazione data?
290300081030         %occur(LdV) = 19;
290400081030         %subst(LdVTxt : 94) = 'Date ' + PdspA;
290500081030
290600101221         // -?Impostazione numero colli?
290700081030         %occur(LdV) = 20;
290800081030         %subst(LdVTxt : 94) = 'Parcel N.' + PnclA;
290900081030
291000101221         // -?Impostazione numero parcel?
291100081030         %occur(LdV) = 21;
291200081030         %subst(LdVTxt : 94) = §B4Ipn;
291300081111
291400131015         // -?"Decodifica" del barcode?
291500081111         %occur(LdV) = 22;
291600081111         %subst(LdVTxt : 94) = *blank;
291700081111         if  §B4Ipn <> *blank;
291800101221           // -?controllo se vecchio o nuovo barcode?
291900081111           if  %subst(§B4Ipn : 14 : 1) = *blank;
292000081111             // - - vecchio
292100081111             %subst(LdVTxt : 94) = '01 ' + %subst(§B4Ipn : 1 : 3)
292200081111                                 + ' '   + %subst(§B4Ipn : 4 : 8);
292300081111           else;
292400081111             // - - nuovo
292500081111             %subst(LdVTxT : 94) = '01 ' + %subst(§B4Ipn : 1 :  4)
292600081111                                 + ' '   + %subst(§B4Ipn : 5 : 10);
292700081111           endif;
292800081111         endif;
292900081111
293000130716         // -?Impostazione costanti  (LB ha detto di NON metterle)?
293100081111         //%occur(LdV) = 23;
293200081111         //%subst(LdVTxt : 94) = *blank;
293300081111         //%occur(LdV) = 24;
293400081111         //%subst(LdVTxt : 94) = 'Controllare la corrispon-';
293500081111         //%occur(LdV) = 25;
293600081111         //%subst(LdVTxt : 94) = 'denza del destinatario con';
293700081111         //%occur(LdV) = 26;
293800081111         //%subst(LdVTxt : 94) = 'l''indirizzo dell''etichetta';
293900081111         //%occur(LdV) = 27;
294000081111         //%subst(LdVTxt : 94) = 'DPD.';
294100081030
294200081030       ENDSR;
294300081030
294400081030       //--------------------------------------------------------------
294500130716       //?Preparazione dati x spedizione "FedEx"                       ?
294600081030       //--------------------------------------------------------------
294700130716       BEGSR  sr_LdVbaseFED;
294800081030
294900120524         // -?Impostazione del modulo FedEx:?
295000131203         //// ·?Modulo FedEx con logo Bartolini?
295100131203         //TemplateName = 'ldvbase03 ';
295200131203         //// ·?Modulo FedEx con logo Bartolini/BRT?
295300131203         //TemplateName = 'ldvbase13 ';
295400120524         // ·?Modulo FedEx con logo BRT?
295500120524         TemplateName = 'ldvbase23 ';
295600110517
295700081103         w_Row  = c_Row_B;
295800081030         Wtrc = 'C';
295900081030
296000101221         // -?Preparazione elenco segnacolli FedEx?
296100101213         if  NOT %open(FNART01L);
296200101213           open(e)  FNART01L;
296300101213           if  %error;
296400101213             exsr  sr_ErrOpenFile;
296500101213           endif;
296600101213         endif;
296700101213         if  NOT %open(FIARS01L);
296800101213           open(e)  FIARS01L;
296900101213           if  %error;
297000101213             exsr  sr_ErrOpenFile;
297100101213           endif;
297200101213         endif;
297300081030
297400101221         // -?Impostazione numeratore fino alla 39ª riga?
297500101213         FOR  wx = wx  TO (w_Row - c_Row_Fed);
297600081030
297700081030           %occur(LdV) = wx;
297800081030           LdvPrg      = wx;
297900081030
298000101213         ENDFOR;
298100081030
298200101221         // -?40ª riga & successive?
298300081030         clear  xx;
298400081030         clear  Wnsc;
298500101221         FOR  wx = (w_Row - c_Row_Fed + 1)  TO w_Row;
298600081030
298700081030           %occur(LdV) = wx;
298800081030           LdvPrg      = wx;
298900081030
299000081030           // - Caricamento AWB FedEx
299100081103           clear  wAWB;
299200101221           // - -?leggo segnacolli per bolle in arrivo?
299300081030           exsr  CarAWB_Arx;
299400081030           if  wAWB = *blank;
299500081030             leave;
299600081030           endif;
299700081030
299800081030           xx    += 1;
299900081103           Pptn   = wAWB;
300000081030           Psts   = %editc( xx : 'Z' );
300100081030           LdvTxt = ds_Fed;
300200081030
300300101221         ENDFOR;
300400081030
300500081030       ENDSR;
300600081030
300700081030       //--------------------------------------------------------------
300800130716       //?Riempimento del box "Promemoria per l'autotrasportore"       ?
300900081030       //--------------------------------------------------------------
301000130716       BEGSR  sr_LdVbase01;
301100081030
301200120524         // -?Impostazione del modulo con Box aggiuntivo (14 rec. in +)?
301300131203         //// ·?Modulo Box aggiuntivo con logo Bartolini?
301400131203         //TemplateName = 'ldvbase01 ';
301500131203         //// ·?Modulo Box aggiuntivo con logo Bartolini/BRT?
301600131203         //TemplateName = 'ldvbase11 ';
301700120524         // ·?Modulo Box aggiuntivo con logo BRT?
301800120524         TemplateName = 'ldvbase21 ';
301900110517
302000081103         w_Row  = c_Row_B;
302100081030
302200101221         // -?Intestazione box "Promemoria per l'autotrasportatore"?
302300131206         //  ?(NON LA SI FA PIÙ)?
302400131206         //exsr  sr_IntestBox;
302500131206
302600131206
302700131206         // -?Posizionamento sulla 36ª riga?
302800131206         //  ?(Il ciclo "FOR" precedente dovrebbe aver già posizionato?
302900131206         //  ? sulla 37ª riga)?
303000131206         wx = 36;
303100131206
303200131206         // -?Se reperito il 1° PASSAGGIO (c/o destinatario):?
303300131206         IF  $1o_Passaggio;
303400131206
303500131206           test(de) EVBdev;
303600131206           if %error;
303700131206             clear w0080;
303800131206           else;
303900131206             Data_Eur = %date( %char(EVBdev) : *iso0 );
304000131206             w0080    = %dec(Data_Eur);
304100131206           endif;
304200131206
304300131206           // -?37ª riga?
304400131206           wx += 1;
304500131206           %occur(LdV) = wx;
304600131206           LdvPrg      = wx;
304700131206           Ppma    = 'Primo passaggio del '
304800131206                   + %editc( w0080 : 'Y' )
304900131206                   + ' effettuato alle '
305000131206                   + %editw( EVBhev: '  :  ' );
305100131206           LdvTxt  = ds_37;
305200131206
305300131206           // -?38ª riga: VUOTA (separatrice)?
305400131206           wx += 1;
305500131206           %occur(LdV) = wx;
305600131206           LdvPrg      = wx;
305700131206           clear  LdvTxt;
305800131206
305900131206         ENDIF;
306000131206
306100081030
306200131206         // -?Se reperite CONSEGNE PARZIALI PRECEDENTI:?
306300131206         IF  $ConsParz = *on;
306400131206
306500131206           // -?Impostazione segnalazione in output?
306600131206           xx = 1;
306700131206           DoW  (xx <= %elem($CP)   and   $CP(xx) < *all'9');
306800131206
306900131206             Elem_CP = $CP(xx);
307000131206             test(de) WCPdcm;
307100131206             if %error;
307200131206               clear w0080;
307300131206             else;
307400131206               Data_Eur = %date( %char(WCPdcm) : *iso0 );
307500131206               w0080    = %dec(Data_Eur);
307600131206             endif;
307700131206
307800131206             // -?37ª o 39ª riga & successive?
307900131206             wx += 1;
308000131206             %occur(LdV) = wx;
308100131206             LdvPrg      = wx;
308200131206             Ppma    = 'Consegnati colli '
308300131206                     + %editc(WCPncl:'Z')
308400131206                     + ' in data '
308500140304                     + %editc(w0080:'Y');
308600140304             if  WCPocm > *zero;
308700140304               Ppma    = %trimr( Ppma )
308800140304                       + ' alle ore '
308900140304                       + %editw( WCPocm : '0 :  ' );
309000140304             endif;
309100140304             if  WCPfir <> *blank;
309200140304               Ppma    = %trimr( Ppma )
309300140303                     + ' - Firmatario: '
309400140303                     + WCPfir;
309500140304             endif;
309600131206             LdvTxt  = ds_37;
309700131206
309800131206             xx += 1;
309900131206
310000131206           EndDo;
310100131206
310200131206         ENDIF;
310300081030
310400081030       ENDSR;
310500080801
310600080801       //--------------------------------------------------------------
310700130716       //?Intestazione del box "Promemoria per l'autotrasportore"      ?
310800080801       //--------------------------------------------------------------
310900131206       //BEGSR  sr_IntestBox;
311000131206       //
311100131206       //  // 37ª riga
311200131206       //  //wx += 1;         (già incrementato dal ciclo "FOR" precedente)?
311300131206       //  %occur(LdV) = wx;
311400131206       //  LdvPrg      = wx;
311500131206       //  Ppma    = '                             '
311600131206       //          + c_PMxA;
311700131206       //  LdvTxt  = ds_37;
311800131206       //
311900131206       //  // -?38ª riga?
312000131206       //  wx += 1;
312100131206       //  %occur(LdV) = wx;
312200131206       //  LdvPrg      = wx;
312300131206       //  Ppma    = *all'=';
312400131206       //  LdvTxt  = ds_37;
312500131206       //
312600131206       //ENDSR;
312700101221
312800080430      /end-free
312900081030      *
313000081030      *---------------------------------------------------------------*
313100081030      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
313200081030      *---------------------------------------------------------------*
313300081030     c     CarAWB_Arx    BEGSR
313400130215      *
313500130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
313600130215      *   comunque una bolla mamma
313700130215     c                   exsr      sr_Mamma_FedEx
313800081030      *
313900081030      * Posizionamento
314000130215if  2c                   if        $Mamma = *off   and
314100130215     c                             $Mamma_FedEx = *off
314200081030     c     KLDVnsc       setgt     FNART000
314300081030x   2c                   else
314400081030     c     KLBLnsc       setgt     FNART000
314500081030e   2c                   endif
314600081030      *
314700081030      * Ciclo di lettura
314800081030do  1c                   DO        *hival
314900081030      *
315000130215if  2c                   if        $Mamma = *off   and
315100130215     c                             $Mamma_FedEx = *off
315200081030     c     KLDV          reade     FNART000
315300081030x   2c                   else
315400081030     c     KLBL          reade     FNART000
315500081030e   2c                   endif
315600081030if  2c                   if        %eof(FNART01L)
315700081030     c                   leave
315800081030e   2c                   endif
315900081030      * - per ogni segnacollo cerco l'AWB
316000081030     c     K05ARS01A     chain     FIARS000
316100081030if  2c                   if            %found(FIARS01L)
316200081030     c                             and %subst(ARSnot:1:1) >= *zeros
316300081030     c                   eval      wAWB = %subst(ARSnot:1:12)
316400081030     c                   eval      wNSC = ARSnsc
316500081030     c                   leave
316600081030e   2c                   endif
316700081030      *
316800081030e   1c                   ENDDO
316900081030      *
317000081030     c                   ENDSR
317100130215      *
317200130215      /free
317300130215
317400130215       //--------------------------------------------------------------
317500130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
317600130215       //--------------------------------------------------------------
317700130215       BEGSR  sr_Mamma_FedEx;
317800130215
317900130924         //$Mamma_FedEx = *off;         ?(già così)?
318000130215
318100130924         If  Not $Mamma;
318200130215
318300130215           // -?Verifica se la spedizione ha comunque mamma?
318400130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
318500130215           if  not %open(FNLBL01L);
318600130215             open  FNLBL01L;
318700130215           endif;
318800130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
318900130215
319000130215           $Mamma_FedEx = %found(FNLBL01L);
319100130215
319200130215         EndIf;
319300130215
319400130215       ENDSR;
319500130215
319600130215      /end-free
319700131008      *
319800131008      *---------------------------------------------------------------*
319900131008      *?Prepara elenco SegnaColli Non Sequenziali (dett.) - MAX 2 -  ?*
320000131008      *?  da FNART (arrivo)                                          ?*
320100131008      *---------------------------------------------------------------*
320200131008     c     ListaScNSd    BEGSR
320300131008      *
320400131008      * SE ci sono solo 2 segnacolli non sequenziali:
320500131008      *   staranno al posto del range - basta non indicare
320600131008      *   "NON SEQUENZIALI"
320700131008      *
320800131008if  1c                   if        not %open(FNART01L)
320900131008     c                   open(e)   FNART01L
321000131008      /free
321100131008           if  %error;
321200131008             exsr  sr_ErrOpenFile;
321300131008           endif;
321400131008      /end-free
321500131008e   1c                   endif
321600131008      *
321700131008      * memorizzo l'ultimo numero di segnacollo
321800131008if  1c                   if        $Mamma = *off
321900131008     c     KLDV          setgt     FNART000
322000131008     c     KLDV          readpe    FNART000
322100131008x   1c                   else
322200131008     c     KLBL          setgt     FNART000
322300131008     c     KLBL          readpe    FNART000
322400131008e   1c                   endif
322500131008if  1c                   if        NOT %eof(FNART01L)
322600131008     c                   eval      %subst(Pnsx : 8) = ' , '
322700131008     c                                              + %editc(ARTnsc : 'Z')
322800131008e   1c                   endif
322900131008      *
323000131008      * memorizzo il primo numero di segnacollo
323100131008if  1c                   if        $Mamma = *off
323200131008     c     KLDV          setll     FNART000
323300131008     c     KLDV          reade     FNART000
323400131008x   1c                   else
323500131008     c     KLBL          setll     FNART000
323600131008     c     KLBL          reade     FNART000
323700131008e   1c                   endif
323800131008if  1c                   if        NOT %eof(FNART01L)
323900131008     c                   eval      %subst(Pnsx : 1 : 7) =
324000131008     c                                 %editc(ARTnsc : 'Z')
324100131008e   1c                   endif
324200131008      *
324300131008     c                   ENDSR
324400030515      *
324500030515      *---------------------------------------------------------------*
324600050217      *?Carico LDV da FNARB00F                                       ?*
324700030515      *---------------------------------------------------------------*
324800110208     c     sr_CarARB     BEGSR
324900030515      *
325000110202      /free
325100110202
325200110202         //____________________________________________________________
325300110202         //? RISULTATO è impostato di default a "OK" => creo immagine ?
325400110202         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
325500110202
325600110202         // »? STEP 1 ?«
325700131203         //  ?Verifica se presenti forzature per la distinta           ?
325800131203         //  ?(vedi file FIIRD00F)                                     ?
325900131203
326000131203         //?-?Verifica se distinta in FIIRD10F                           ?
326100131203         //  ?e se elaborabile con (IRDTPELAB = *blank) o senza controlli?
326200130716         chain  %kds(k01fiird11)  FIIRD000;
326300130313
326400130716         If  %found(FIIRD11L)  and  IRDtpElab = *blank;
326500131203           // -?SE rileva errori =>?
326600131203           // -?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
326700130716           exsr  sr_STEP_1_CtrlTASFLO;
326800130716         EndIf;
326900131203
327000131203         // »? STEP 2 ?«
327100131203         //  ?Verifica se spedizione elaborabile                       ?
327200131203         //  ?(controlli su distinta e spedizione nel file FIILV00F)   ?
327300131203         //  ?- SE distinta SENZA forzature in FIIRD00F -              ?
327400110202
327500110204         // -?Reperimento dati distinta (FIDST)?
327600110228         clear  dDstflr;
327700110202         k_DSTnpg = 4;
327800110202         k_DSTnfv = ds_Distinta.p_Ndc;
327900110202         k_DSTfgs = ds_Distinta.p_Fgs;
328000110202         chain  %kds(k03fidst01)  FIDST000;
328100110228         if  %found(FIDST01L);
328200110228           dDSTflr = DSTflr;
328300110228         endif;
328400110202
328500110202         // -?Controllo distinta:?
328600130920         //?·?(SE Distinta NON trovata in FIIRD10F per recupero immagini?
328700130924         //  ?  & NON è una spedizione FORZATA PER TEST)                ?
328800131016         //  ?SE distinta NON trovata in FIDST01L: registra LOG e prosegue?
328900131016         //  ?SE distinta NON chiusa:?
329000131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
329100130716         If  Not %found(FIIRD11L)  and
329200130716             $Test = *blank;
329300131203           exsr  sr_STEP_2_CtrlDISTINTA;
329400130716         EndIf;
329500110202
329600110202
329700110202         // -?Reperimento dati Immagine LdV (FIILV)?
329800110208         clear  ILVcol;
329900110309         clear  wCountRec01;
330000110202         chain(e)  %kds(k06fiilv01)  FIILV000;
330100130313
330200130716         //?·?DISTINTA in FIIRD10F => recupero immagine (senza ctrl)?
330300131016         //?·?DISTINTA NON in FIIRD10F => controlli da eseguire     ?
330400131016         //  ?SE trovati errori:?
330500131016         //  ?"risponde" ed eventualmente esce dalla s.p. (direttamente dalla subr.)?
330600130716         If  Not  %found(FIIRD11L);
330700131203           exsr  sr_STEP_2_CtrlFIILV;
330800130716         EndIf;
330900130313
331000131203         // »? STEP 3 ?«
331100131203         //  ?Controllo spedizione                                     ?
331200131203         //  ?(da eseguire comunque)                                   ?
331300110202
331400131203         // -?Reperimento dati Spedizione (FNARB)?
331500110207         chain  KLDV  FNARB000;
331600101220
331700110202         // -?Spedizione non trovata?
331800101221         if  NOT %found(FNARB01L);
331900130924           // ·?Spedizione non trovata => ERRORE: non rielaborare immagine?
332000131203           //  ?"risponde" ed esce dalla s.p.?
332100110204           Risultato = 'XX';
332200130924           //RtnOpCode = 'FNARB_MANC';
332300130924           //RtnEsito  = -1;
332400130924           // -?Registrazione log in FIILV, differenziando l'errore per?
332500130924           //  ?iter normale (25) o iter con forzatura (52)?
332600130924           if  Not  %found(FIIRD11L);
332700130313             wCod_Log = '25';
332800130716           else;
332900130716             wCod_Log = '52';
333000130716           endif;
333100110208           exsr  sr_WrtFIILV;
333200101221           // -?Uscita dalla s.p.?
333300110204           exsr  sr_End;
333400101221         endif;
333500101221
333600101221
333700110202         // -?Registrazione record per spedizione "OK" (root "buono")?
333800110218         If  wCod_Log = *blank;
333900130206           RootFolder = §ILVimgOk;
334000110203           wCod_Log = '01';
334100110202           exsr  sr_WrtFIILV;
334200110218         EndIf;
334300090121
334400080523      /end-free
334500070514      *
334600070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
334700070514      *   il reperimento dell'eventuale bolla mamma...
334800070514     c                   eval      wB0ntc =  ARBntc
334900120524     c                   eval      wB0dcp =  ARBdcp
335000120524     c                   eval      wB0tmc =  ARBtmc
335100120524     c                   move      ARBpdc        Wpdc4a
335200080523     c                   eval      wB0dcm =  ARBdcm
335300121018     c                   eval      wB0piv =  ARBcpi
335400030515      *
335500030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
335600030515     c                   exsr      sr_DS_ARB
335700030515      *
335800070308      * Decodifico filiale di Arrivo
335900131203     c                   exsr      sr_DecLNA
336000030515      *
336100030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
336200030515      *    "generiche" (3A, TB, 5E)
336300030515     c                   exsr      sr_DS_TAB
336400030515      *
336500030515      * Ultimi dati mancanti:
336600030515      * - Cod. cliente mittente
336700030515if  1c                   If        ARBccm <> *zeros
336800030515     c                   movel     ARBccm        DB0ccm
336900030515x   1c                   Else
337000030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
337100030515     c                   movel     ARBksc        DB0ccm
337200030515e   2c                   endif
337300030515e   1c                   EndIf
337400030515      * - Cod. cliente destinatario
337500030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
337600030515     c                   movel     ARBksc        DB0ccd
337700030515e   1c                   endif
337800030515      * - Se c'è 2ª bolla imposto LNA9999
337900030515if  1c                   if        DB0tb2 <> *blanks
338000030515     c                   eval      DB0ccd = *all'9'
338100030515     c                   movel     ARBlna        DB0ccd
338200030515e   1c                   endif
338300120524      * - Reperimento Data spedizione
338400120524if  1c                   if        DB0mgs <> *zeros
338500120524     c                   z-add     DB0mgs        Wdsp
338600120524     c                   movel     DB0aas        Wdsp
338700120524x   1c                   else
338800120524      *   prendo la data del giorno se non c'è la data spedizione
338900120524     c                   time                    w0140
339000120524     c                   move      w0140         w0080
339100120524     c     *eur          movel     w0080         Data_EUR
339200120524     c     *iso          movel     Data_EUR      Wdsp
339300120524e   1c                   endif
339400030515      *
339500061030      * 3ª impostaz. campi della DS: importi da calcolare
339600061031     c                   exsr      sr_DS_IMPO
339700030515      *
339800080505     c                   ENDSR
339900080724      /free
340000130716
340100130716       //--------------------------------------------------------------
340200130716       //?»  STEP 1  «                                                 ?
340300130716       //?Distinta in FIIRD10F e senza forzature (IRDTPLAB = *blank):  ?
340400130716       //?verifica se immagine già generata (DTASFLO.§FLOIML <> *blank)?
340500130716       //--------------------------------------------------------------
340600130716       BEGSR  sr_STEP_1_CtrlTASFLO;
340700130716
340800130716         // -?Reperimento spedizione da TITAS per verificarne lo stato?
340900131016         //  ?della relativa immagine.  Se già presente immagine:?
341000131016         //  ?"risponde" con codice errore 51 e "non elaborare"?
341100131016         //  ?ed esce dalla s.p.?
341200130716         if  %subst( wSysName : 1 : 6 ) = 'SETRAS';
341300130716           wLibFileTITAS = 'GAITRAGRU/TITAS30C';
341400130716         else;
341500130716           wLibFileTITAS = 'GAITRAGRPS/TITAS30C';
341600130716         endif;
341700130716         if  NOT  %open(TITAS30C);
341800130716           open(e)  TITAS30C;
341900130716           if  %error;
342000130716             exsr  sr_ErrOpenFile;
342100130716           endif;
342200130716         endif;
342300130716
342400130716         chain  %kds(k05titas30 : 4)  TITAS30C;
342500130716
342600130716         If  %found(TITAS30C)            and
342700130716             TASdcm > *zero              and
342800130716             TASndc = ds_Distinta.p_Ndc  and
342900130716             %int( TASpdc / 10000 ) = ds_Distinta.p_Fgs;
343000130716
343100130716           dTASflo = TASflo;
343200130716
343300130716           if  dTASflo.§FLOiml <> *blank;
343400130716             Risultato = 'XX';
343500130924             //RtnOpCode = 'GIÀ_IMMAG ';
343600130924             //RtnEsito  = -1;
343700130716             // -?Registrazione log in FIILV?
343800130716             wCod_Log = '51';
343900130716             exsr  sr_WrtFIILV;
344000130716             // -?Uscita dalla s.p.?
344100130716             exsr  sr_End;
344200130716           endif;
344300130716
344400130716         EndIf;
344500130716
344600130716       ENDSR;
344700130716
344800130716       //--------------------------------------------------------------
344900131203       //?»  STEP 2  «                                                 ?
345000131203       //?Distinta NON in FIIRD10F:?controlli sulla distinta.          ?
345100130716       //--------------------------------------------------------------
345200131203       BEGSR  sr_STEP_2_CtrlDISTINTA;
345300130716
345400130716         Select;
345500130716
345600130716           // ·?DISTINTA non trovata => ANOMALIA?
345700130716           When  NOT  %found(FIDST01L)  or
345800130716                 DSTatb <> *blank;
345900130924             // -?Registrazione log in FIILV - se non già fatto?
346000130716             if  wCod_Log = *blank;
346100130716               wCod_Log = '22';
346200130716               exsr  sr_WrtFIILV;
346300130716             endif;
346400130716
346500131016           // ·?DISTINTA non chiusa => ripetere la richiesta?
346600130716           When  DSTfcf = *blank;
346700130716             Risultato  = 'NO';
346800130924             //RtnOpCode  = 'DIST_APERT';
346900130716             //RtnEsito   = -1;
347000130716             // -?Uscita dalla s.p.?
347100130716             exsr  sr_End;
347200130716
347300130716         EndSl;
347400130716
347500130716       ENDSR;
347600130716
347700130716       //--------------------------------------------------------------
347800131203       //?»  STEP 2  «                                                 ?
347900131203       //?Distinta NON in FIIRD10F:?controlli sul/i record in FIILV00F.?
348000130716       //--------------------------------------------------------------
348100131203       BEGSR  sr_STEP_2_CtrlFIILV;
348200130716
348300130716         Select;
348400130716
348500130716           // ·?Rec. IMMAGINE LdV allocato => ripetere la richiesta?
348600130716           When  %error;
348700130716             Risultato  = 'NO';
348800130924             //RtnOpCode  = 'FIILV_ALLC';
348900130716             //RtnEsito   = -1;
349000130716             // -?Uscita dalla s.p.?
349100130716             exsr  sr_End;
349200130716
349300130716           // ·?Rec. IMMAGINE LdV non trovato?
349400130716           When  NOT  %found(FIILV01L);
349500130716             select;
349600130716               // ·?DISTINTA senza scarico a PDA delle spedizioni?
349700130716               //  ?=> cancellare immagine (senza LOG)?
349800130716               when  DSTpda <> 'C'   and   DSTpda <> 'E';
349900130716                 Risultato  = 'XX';
350000130924                 //RtnOpCode  = 'DIST_NOPDA';
350100130716                 //RtnEsito   = -1;
350200130716                 // -?Uscita dalla s.p.?
350300130716                 exsr  sr_End;
350400130716               // ·?DISTINTA in test consegne?
350500130716               //  ?=> cancellare immagine (senza LOG)?
350600130716               when  §DSTtstPda = 'C'   or   §DSTtstPda = 'E';
350700130716                 Risultato  = 'XX';
350800130924                 //RtnOpCode  = 'DIST_TEST ';
350900130716                 //RtnEsito   = -1;
351000130716                 // -?Uscita dalla s.p.?
351100130716                 exsr  sr_End;
351200130716               // ·?Altrimenti: ANOMALIA?
351300130924               //  ?Registrazione log in FIILV - se non già fatto?
351400130716               other;
351500130716                 if  wCod_Log = *blank;
351600130716                   wCod_Log = '23';
351700130716                   exsr  sr_WrtFIILV;
351800130716                 endif;
351900130716             endsl;
352000130716
352100130716           // ·?Rec. IMMAGINE LdV già elaborato?
352200130716           //  ?=> ricerca record NON elaborato (ILVCOL = "00")?
352300130716           When  ILVcol <> '00';
352400130716             exsr  sr_CercaLog00;
352500130716             // ·?Rec. IMMAGINE LdV non trovato (né "00" né "01")?
352600130924             //  ?Registrazione log in FIILV - se non già fatto?
352700130716             if  ILVcol <> '00'  and  wCountRec01 = *zero
352800130716                                 and  wCod_Log    = *blank;
352900130716               wCod_Log = '23';
353000130716               exsr  sr_WrtFIILV;
353100130716             endif;
353200130716
353300130716         EndSl;
353400130716
353500130716
353600130716         // -?Verifica SE trovato record da elaborare per la sped/dist?
353700130716         $FIILV = (ILVcol = '00');
353800130716
353900130716         // -?Impostazione struttura dati del campo ILVLIBERO?
354000130716         dILVlibero = ILVlibero;
354100130716
354200130716
354300130716         Select;
354400130716
354500130716           // -?Spedizione non consegnata in distinta?
354600130716           When  ILVnoCDst = 'S';
354700130924             // -?Registrazione log in FIILV - se non già fatto?
354800130716             if  wCod_Log = *blank;
354900130716               wCod_Log = '08';
355000130716               exsr  sr_WrtFIILV;
355100130716             endif;
355200130716
355300130716           // -?LdV da scannerizzare?
355400130716           When  ILVscan = *blank;
355500130924             // -?Registrazione log in FIILV - se non già fatto?
355600130716             if  wCod_Log = *blank;
355700130716               wCod_Log = '09';
355800130716               exsr  sr_WrtFIILV;
355900130716             endif;
356000130716
356100130716         EndSl;
356200130716
356300130716       ENDSR;
356400130716
356500130716       //--------------------------------------------------------------
356600130716       //?Ricerca record IMMAGINE LDV NON elaborato (ILVCOL = "00")    ?
356700130716       //--------------------------------------------------------------
356800130716       BEGSR  sr_CercaLog00;
356900130716
357000130716         //?N.B.?Il 1° rec. per spedizione/distinta è già stato letto?
357100130716
357200130716         DoU  ILVcol = '00';
357300130716
357400130716           // ·?Rec. Immagine LdV già elaborato ("01")?
357500130716           if  ILVcol = '01';
357600130716             wCountRec01 += 1;
357700130716           endif;
357800130716
357900130716           reade(e)  %kds(k06fiilv01)  FIILV000;
358000130716
358100130716           select;
358200130716
358300130716             // ·?Rec. Immagine LdV allocato => ripetere la richiesta?
358400130716             when  %error;
358500130716               Risultato  = 'NO';
358600130924               //RtnOpCode  = 'FIILV_ALLC';
358700130716               //RtnEsito   = -1;
358800130716               // -?Uscita dalla s.p.?
358900130716               exsr  sr_End;
359000130716
359100130716             // ·?Solo rec. Immagine LdV già elaborati?
359200130716             //  ?=> scrittura nuovo?
359300130716             when  %eof(FIILV01L);
359400130716               clear  ILVcol;
359500130716               leave;
359600130716
359700130716             // ·?Già reperiti 100 rec. per le stesse sped./dist.?
359800130716             //  ?(questo è in realtà il 101°)?
359900131016             //  ?=> NON ripetere la richiesta (senza LOG)?
360000130716             when  wCountRec01 >= 100;
360100130716               Risultato  = 'XX';
360200130924               //RtnOpCode  = 'SPEDGIÀ100';
360300130716               //RtnEsito   = -1;
360400130716               // -?Invio e-mail?
360500130716               exsr  sr_SendMAIL;
360600130716               // -?Uscita dalla s.p.?
360700130716               exsr  sr_End;
360800130716
360900130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
361000130716             //  ?creazione immagine sul PDA successive all'attuale?
361100131016             //  ?=> NON ripetere la richiesta (senza LOG)?
361200130716             //when  DataOraCrea <= ILVprmTimP;
361300130716             when  DataOraCrea < ILVprmTimP;
361400130716               Risultato  = 'XX';
361500130924               //RtnOpCode  = 'SPED_GIÀEL';
361600130716               //RtnEsito   = -1;
361700130716               // -?Uscita dalla s.p.?
361800130716               exsr  sr_End;
361900130716
362000130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
362100130716             //  ?creazione immagine sul PDA identica all'attuale?
362200130716             //  ?=> riprovare dopo 10 minuti (senza LOG)?
362300130716             when  DataOraCrea = ILVprmTimP  and
362400130716                   ILVtimA > %int( %subst( %char( %dec(
362500130716                                   %timestamp() - %minutes(10) ) )
362600130716                                           : 1 : 14 ) );
362700130716               Risultato  = 'NO';
362800130924               //RtnOpCode  = '..TRA10MIN';
362900130716               //RtnEsito   = -1;
363000130716               // -?Uscita dalla s.p.?
363100130716               exsr  sr_End;
363200130716
363300130716           endsl;
363400130716
363500130716         EndDo;
363600130716
363700130716       ENDSR;
363800101221
363900080724       //--------------------------------------------------------------
364000130716       //?Registrazione record nel work-file FIILV00F                  ?
364100080724       //--------------------------------------------------------------
364200110207       BEGSR  sr_WrtFIILV;
364300110202
364400110202         // -?Segnalazione anomalia rilevata per la spedizione?
364500110204         if  Not  $FIILV;
364600110202           clear  FIILV000;
364700110202           ILVfgs = ds_Distinta.p_Fgs;
364800110202           ILVndc = ds_Distinta.p_Ndc;
364900110202           ILVaas = DB0aas;
365000110202           ILVlnp = DB0lnp;
365100110202           ILVnrs = DB0nrs;
365200110202           ILVnsp = DB0nsp;
365300110202         endif;
365400110202
365500110207         ILVcol     = wCod_Log;
365600110202         xx = %lookup( wCod_Log : $CoL );
365700110202         if  xx > *zero;
365800110202           ILVnot   = $DeL(xx);
365900110202         endif;
366000110202         ILVprmSpe  = Spedizione;
366100110202         ILVprmDst  = Distinta;
366200110202         ILVprmTimp = DataOraCrea;
366300110202         ILVrisult  = Risultato;
366400110202         ILVtimA    = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
366500130411
366600130411
366700131203         // -? Spedizioni FORZATE PER TEST => NON scrive il rec.       ?
366800130411         IF  $Test <> *blank;
366900130411           leavesr;
367000130411         ENDIF;
367100130411
367200090210
367300110923
367400131203         // -?S.P. richiamata per stampa cartacea => NON scrive il rec.?
367500110923         If  SDSprm > 11;
367600110923           leavesr;
367700110923         EndIf;
367800110923
367900110923
368000110204         if  $FIILV;
368100110202           //_______________
368200110202           update  FIILV000;
368300110202           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
368400110202         else;
368500110204           //_______________
368600110204           write   FIILV000;
368700110204           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
368800110202         endif;
368900080724
369000080724       ENDSR;
369100130227
369200130227       //--------------------------------------------------------------
369300130716       //?Invio e-mail per avvisare degli oltre 100 rec. con cod. LOG  ?
369400130716       //?   "01" per la stessa spedizione in FIILV.                   ?
369500130227       //--------------------------------------------------------------
369600130227       BEGSR  sr_SendMAIL;
369700130227
369800130227         // -?Reperimento tab. "MRA"?
369900130227         reset  dMRAdan;
370000130227         reset  TIBS02ds;
370100130227         T02cod = 'MRA';
370200130227         T02ke1 = SDSpgm;
370300130227         T02ke2 = *blank;
370400130227         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
370500130227         if  T02err = *blank;
370600130227           dMRAdan = T02uni;
370700130227         endif;
370800130227
370900130227         // -?Override ai file di stampa?
371000130227         //  ?(per impostarvi i dati per l'invio via e-mail)?
371100130227         exsr sr_OvrPrtF;
371200130227
371300130227         // -?Stampa segnalazione?
371400130227         except  EmlTxt;
371500130227         P_Fgs2 = ds_Distinta.p_Fgs;
371600130227         P_Ndc2 = ds_Distinta.p_Ndc;
371700130227         except  EmlDet;
371800130227         except  EmlEnd;
371900130227
372000130227         // -?Cancellazione Override ai file di stampa?
372100130227         exsr sr_DeleteOvr;
372200130227
372300130227       ENDSR;
372400130227
372500130227       //--------------------------------------------------------------
372600130716       //?Override al file di stampa per impostarvi i dati per         ?
372700130716       //?  l'invio via e-mail.                                        ?
372800130227       //--------------------------------------------------------------
372900130227       BEGSR  sr_OvrPrtF;
373000130227
373100130227         reset  TRTCM1ds;
373200130227         if  §MRAdmitt <> *blank;
373300130227           §CM1mitt = %trim(§MRAdmitt);
373400130227         endif;
373500130227         §CM1dst  = %trim(§MRAddest);
373600130227         §CM1tips = §MRAdreg;
373700130227         //§CM1var  = '*OBJM*' + §MRAddes;
373800130227         §CM1idp  = §MRAdidpro;
373900130227
374000130227         // -?Override al "corpo e-mail"?
374100130227         if  §MRAdreg <> *blank;
374200130227           Qcmd = c_CmdOvrPrtF
374300130227                + ' outq(' + %trim(§MRAdoutqi) + ')'
374400130227                + ' usrdfndta(''' + TRTCM1ds + ''')';
374500130227         else;
374600130227           Qcmd = c_CmdOvrPrtF;
374700130227         endif;
374800130227
374900130227         exsr  sr_ExecCmd;
375000130227
375100130227         //if  Qusei <> *blank;
375200130227         //  ...;
375300130227         //endif;
375400130227
375500130227         // -?Apertura file di stampa?
375600130227         open  PrtEmail;
375700130227
375800130227       ENDSR;
375900130227
376000130227       //--------------------------------------------------------------
376100130227       //?Chiusura file di stampa e cancellazione override.            ?
376200130227       //--------------------------------------------------------------
376300130227       BEGSR  sr_DeleteOvr;
376400130227
376500130227         // -?Chiusura file di stampa?
376600130227         if  %open(PrtEMAIL);
376700130227           close  PrtEMAIL;
376800130227         endif;
376900130227
377000130227         // -?Cancellazione override al file di stampa?
377100130227         Qcmd = c_CmdDltOvr;
377200130227         exsr  sr_ExecCmd;
377300130227
377400130227       ENDSR;
377500130227
377600130227       //--------------------------------------------------------------
377700130227       //?Esecuzione del comando (già impostato).                      ?
377800130227       //--------------------------------------------------------------
377900130227       BEGSR  sr_ExecCmd;
378000130227
378100130227         clear Qcap0100;
378200130227         Qcabcsdh = *off;
378300130227         Qcapa    = *off;
378400130227         Qcacmdss = *off;
378500130227         Qcaerved = *allX'00';
378600130227
378700130227         clear Qusec;
378800130227         Qusbprv  = %size(Qusec);
378900130227
379000130227         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
379100130227                           %size(Qcap0100) : 'CPOP0100' : *omit :
379200130227                           0 : 0 : Qusec);
379300130227
379400130227         //if  Qusei <> *blank;
379500130227         //  ...;
379600130227         //endif;
379700130227
379800130227       ENDSR;
379900101221
380000080724      /end-free
380100030515      *
380200030515      *---------------------------------------------------------------*
380300050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
380400030515      *---------------------------------------------------------------*
380500030515     c     sr_DS_ARB     BEGSR
380600030515      *
380700030515     c                   eval      DB0cbo = ARBcbo
380800030515     c                   eval      DB0tsp = ARBtsp
380900030515     c                   eval      DB0mgs = ARBmgs
381000030515     c                   eval      DB0tc1 = ARBtc1
381100030515     c                   eval      DB0tc2 = ARBtc2
381200030515     c                   eval      DB0fns = ARBfns
381300030515     c                   eval      DB0rmn = ARBrmn
381400030515     c                   eval      DB0rma = ARBrma
381500030515     c                   eval      DB0ias = ARBias
381600030515     c                   eval      DB0vas = ARBvas
381700030515     c                   eval      DB0vmd = ARBvmd
381800030515     c                   eval      DB0vad = ARBvad
381900030515     c                   eval      DB0qft = ARBqft
382000030515     c                   eval      DB0lna = ARBlna
382100030515     c                   eval      DB0znc = ARBznc
382200030515     c                   eval      DB0rsm = ARBrsm
382300030515     c                   eval      DB0inm = ARBinm
382400030515     c                   eval      DB0cam = ARBcam
382500030515     c                   eval      DB0lom = ARBlom
382600030515     c                   eval      DB0prm = ARBprm
382700030515     c                   eval      DB0nzm = ARBnzm
382800030515     c                   eval      DB0fap = ARBfap
382900030515     c                   eval      DB0rmo = ARBrmo
383000030515     c                   eval      DB0rsd = ARBrsd
383100030515     c                   eval      DB0ind = ARBind
383200030515     c                   eval      DB0cad = ARBcad
383300030515     c                   eval      DB0lod = ARBlod
383400030515     c                   eval      DB0prd = ARBprd
383500030515     c                   eval      DB0nzd = ARBnzd
383600030515     c                   eval      DB0fin = ARBfin
383700030515     c                   eval      DB0gc1 = ARBgc1
383800030515     c                   eval      DB0gc2 = ARBgc2
383900030515     c                   eval      DB0nas = ARBnas
384000030515     c                   eval      DB0ncl = ARBncl
384100030515     c                   eval      DB0pkf = ARBpkf
384200030515     c                   eval      DB0fvf = ARBfvf
384300030515     c                   eval      DB0vlf = ARBvlf
384400030515     c                   eval      DB0fls = ARBfls
384500030515     c                   eval      DB0ncd = ARBncd
384600030515     c                   eval      DB0nca = ARBnca
384700030515     c                   eval      DB0xco = ARBxco
384800030515     c                   eval      DB0ffd = ARBffd
384900030515     c                   eval      DB0dcr = ARBdcr
385000030515     c                   eval      DB0hcr = ARBhcr
385100030515     c                   eval      DB0tcr = ARBtcr
385200030515     c                   eval      DB0fbr = ARBfbr
385300030515     c                   eval      DB0gga = ARBgga
385400030515     c                   eval      DB0gma = ARBgma
385500030515     c                   eval      DB0gva = ARBgva
385600030515     c                   move      ' '           DB0gva
385700030515     c                   clear                   DB0sop
385800030515     c                   eval      DB0ctr = ARBctr
385900030515     c                   clear                   DB0drt
386000030515     c                   clear                   DB0fpp
386100030515     c                   clear                   DB0nrt
386200030515     c                   clear                   DB0pdr
386300030515      *
386400030515      * Allega documento di trasporto
386500060524      * & Codice cliente da Assegnato
386600130924     c***?(già fatto:)?  clear                   dsBL4A
386700031029     c                   clear                   DB0bam
386800061107     c                   exsr      RepAR4_A
386900060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
387000031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
387100030515     c                   movel     'S'           DB0bam
387200060505     c                   endif
387300030515      *
387400030515      * Destinatario: vedo se esiste 2ª ragione sociale
387500061107     c                   clear                   DB0rd2
387600061107     c                   exsr      RepAR4_D
387700030515      *
387800030515      * Note
387900061107     c                   clear                   DB0nt1
388000061107     c                   clear                   DB0nt2
388100061107     c                   exsr      RepAR4_89
388200030515      *
388300030515     c                   ENDSR
388400030515      *
388500030515      *---------------------------------------------------------------*
388600050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
388700050217      *?   "generiche" (non da "1A")                                 ?*
388800030515      *---------------------------------------------------------------*
388900030515     c     sr_DS_TAB     BEGSR
389000030515      *
389100030515      * Carico le tabelle "generiche" (3A, TB, 5E)
389200030515     c                   exsr      CarTAB
389300030515      *
389400030515      * Codici bolla
389500030515     c                   eval      DB0tb1 = §3Atb1
389600030515     c                   eval      DB0tb2 = §3Atb2
389700030515     c                   eval      DB0dcb = §3Ades
389800030515      * Tipo servizio
389900030515if  1c                   if        DB0dts = *blanks
390000030515     c                   eval      DB0dts = §5Edes
390100030515     c                   eval      DB0fst = §5Efst
390200030515e   1c                   endif
390300030515      * Consegne particolari
390400030515if  1c                   if            DB0tc1 <> *blanks
390500030515     c                             and DB0dc1 =  *blanks
390600030515     c                   movel(p)  DB0tc1        TBLkey
390700030515     c                   exsr      Tab_1P
390800030515     c                   eval      DB0dc1 = §1Pdes
390900030515e   1c                   endif
391000030515if  1c                   if            DB0tc2 <> *blanks
391100030515     c                             and DB0dc2 =  *blanks
391200030515     c                   movel(p)  DB0tc2        TBLkey
391300030515     c                   exsr      Tab_1P
391400030515     c                   eval      DB0dc2 = §1Pdes
391500030515e   1c                   endif
391600030515      * Esenzione IVA
391700030515if  1c                   if            DB0cei <> *blanks
391800030515     c                             and DB0dce =  *blanks
391900030515     c                   eval      DB0dce =  §EIdes
392000030515e   1c                   endif
392100030515      *
392200030515     c                   ENDSR
392300060524      *
392400060524      *---------------------------------------------------------------*
392500060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
392600061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
392700060524      *---------------------------------------------------------------*
392800060524     c     RepAR4_A      BEGSR
392900060524      *
393000130924     c***?(già fatto:)?  clear                   dsBL4A
393100061107      *
393200060524     c                   eval      Wtrc = 'A'
393300060524     c     KLDVtrc       chain     FIAR4000
393400060524      *
393500060524if  1c                   if        %found(FIAR401L)
393600060524     c                   movel     AR4Not        dsBL4A
393700060524e   1c                   endif
393800060524      *
393900060524     c                   ENDSR
394000061107      *
394100061107      *---------------------------------------------------------------*
394200061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
394300061107      *?(2ª ragione sociale del destinatario)                        ?*
394400061107      *---------------------------------------------------------------*
394500061107     c     RepAR4_D      BEGSR
394600061107      *
394700130924     c***?NON esiste:?   clear                   dsBL4D
394800130924     c***?(già fatto:)?  clear                   DB0rd2
394900061107      *
395000061107     c                   eval      Wtrc = 'D'
395100061107     c     KLDVtrc       chain     FIAR4000
395200061107      *
395300061107if  1c                   if        %found(FIAR401L)
395400061107     c                   movel     AR4not        DB0rd2
395500061107e   1c                   endif
395600061107      *
395700061107     c                   ENDSR
395800061107      *
395900060626      *---------------------------------------------------------------*
396000060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
396100061107      *?(dati DPD)                                                   ?*
396200060626      *---------------------------------------------------------------*
396300061030     c     RepAR4_I      BEGSR
396400061030      *
396500130924     c***?(già fatto:)?  clear                   dsBL4I
396600061107      *
396700061030     c                   eval      Wtrc = 'I'
396800061030     c     KLDVtrc       chain     FIAR4000
396900061030      *
397000061030if  1c                   if        %found(FIAR401L)
397100061030     c                   movel     AR4not        dsBL4I
397200060626e   1c                   endif
397300061030      *
397400061030     c                   ENDSR
397500071008      *
397600071015      *---------------------------------------------------------------*
397700071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
397800110204      *?(dati aggiuntivi per O.R.M. abbinato alla spedizione)        ?*
397900071015      *---------------------------------------------------------------*
398000071015     c     RepAR4_M      BEGSR
398100071015      *
398200130924     c***?(già fatto:)?  clear                   dsBL4M
398300071015      *
398400071015     c                   eval      Wtrc = 'M'
398500071015     c     KLDVtrc       chain     FIAR4000
398600071015      *
398700071015if  1c                   if        %found(FIAR401L)
398800071015     c                   movel     AR4Not        dsBL4M
398900071015e   1c                   endif
399000071015      *
399100071015     c                   ENDSR
399200121018      *
399300121018      *---------------------------------------------------------------*
399400121018      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
399500121018      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
399600121018      *---------------------------------------------------------------*
399700121018     c     RepAR4_Q      BEGSR
399800121018      *
399900130924     c***?(già fatto:)?  clear                   dTA4Q
400000130924     c***?(già fatto:)?  clear                   DB0cpi
400100121018      *
400200121018     c                   eval      Wtrc = 'Q'
400300121018     c     KLDVtrc       chain     FIAR4000
400400121018      *
400500121018if  1c                   if        %found(FIAR401L)
400600121018     c                   movel     AR4not        dTA4Q
400700121018e   1c                   endif
400800121018      *
400900121018     c                   ENDSR
401000080523      /free
401100080523
401200080523       //--------------------------------------------------------------
401300080523       //?Reperimento dati da FIAR400F x trk "1"                       ?
401400080523       //?(nominativo del destinatario)                                ?
401500080523       //--------------------------------------------------------------
401600080523       BEGSR  RepAR4_1;
401700080523
401800101222         //clear  dsBL41;        *** NON esiste ***?
401900080523
402000080523         Wtrc = '1';
402100080523         chain  KLDVtrc  FIAR4000;
402200080523
402300080523         if  NOT  %found(FIAR401L);
402400080523           clear  AR4not;
402500080523         endif;
402600080523
402700150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
402800081104           Pfedkcn = AR4not;
402900081104         else;
403000081104           Pkcn    = AR4not;
403100081104         endif;
403200080523
403300080523       ENDSR;
403400080523
403500080523      /end-free
403600061107      *
403700061107      *---------------------------------------------------------------*
403800061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
403900061107      *?(note)                                                       ?*
404000061107      *---------------------------------------------------------------*
404100061107     c     RepAR4_89     BEGSR
404200061107      *
404300130924     c***?NON esiste:?   clear                   dsBL48
404400130924     c***?NON usata:?    clear                   dsBL49
404500130924     c***?(già fatto:)?  clear                   DB0nt1
404600130924     c***?(già fatto:)?  clear                   DB0nt2
404700061107      *
404800061107     c                   eval      Wtrc = '8'
404900061107     c     KLDVtrc       chain     FIAR4000
405000061107if  1c                   if        %found(FIAR401L)
405100061107     c                   movel     AR4not        DB0nt1
405200061107e   1c                   endif
405300061107      *
405400061107     c                   eval      Wtrc = '9'
405500061107     c     KLDVtrc       chain     FIAR4000
405600061107if  1c                   if        %found(FIAR401L)
405700061107     c                   movel     AR4not        DB0nt2
405800061107e   1c                   endif
405900061107      *
406000061107     c                   ENDSR
406100030515      *
406200030515      *---------------------------------------------------------------*
406300050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
406400030515      *---------------------------------------------------------------*
406500030515     c     CarTAB        BEGSR
406600030515      *
406700030515      * Tab. "3A" - Codici Bolla
406800030515      * (NO se rimesso mittente)
406900030515     c                   clear                   ds3A
407000030515if  1c                   if        DB0ris <> 'M'
407100030515     c                   movel     '3A'          TBLcod
407200030515     c                   movel(p)  DB0cbo        TBLkey
407300030515     c     K03TAB        chain     TABEL
407400030515if  2c                   if        %found(TABEL00F)
407500030515     c                   movel     TBLuni        ds3A
407600030515e   2c                   endif
407700030515e   1c                   endif
407800070515     c                   eval      $Mamma =  (§3Abcm = 'S')
407900030515      *
408000030515      * Tab. "TB" - Tipi Bolla
408100030515      * (NO se rimesso mittente)
408200030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
408300030515      * se no non stampo i dati fattura
408400030515     c                   clear                   dsTB
408500030515if  1c                   if        DB0ris <> 'M'
408600030515     c                   movel     'TB'          TBLcod
408700121029if  2c                   if        §3Atb2 <> *blanks
408800030515     c                   movel(p)  §3Atb2        TBLkey
408900030515x   2c                   else
409000030515     c                   movel(p)  §3Atb1        TBLkey
409100030515e   2c                   endif
409200030515     c     K03TAB        chain     TABEL
409300030515if  2c                   if        %found(TABEL00F)
409400030515     c                   movel     TBLuni        dsTB
409500030515e   2c                   endif
409600030515e   1c                   endif
409700030515      *
409800030515      * Tab. "5E" - Tipi Servizio
409900030515     c                   clear                   ds5E
410000090514if  1c                   if        DB0tsp <> *blanks
410100030515     c                   movel     '5E'          TBLcod
410200030515     c                   movel(p)  DB0tsp        TBLkey
410300030515     c     K03TAB        chain     TABEL
410400030515if  2c                   if        %found(TABEL00F)
410500030515     c                   movel     TBLuni        ds5E
410600030515e   2c                   endif
410700030515e   1c                   endif
410800030515      *
410900030911      * Tab. "EI" - Esenzioni I.V.A.
411000030515     c                   clear                   dsEI
411100030911      * Tab. "1A" - Tipi incasso contrassegno
411200030911     c                   clear                   ds1A
411300030911      * Tab. "1P" - Consegne particolari
411400030911     c                   clear                   ds1P
411500030515      *
411600030515     c                   ENDSR
411700030911      *
411800030911      *---------------------------------------------------------------*
411900050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
412000030911      *---------------------------------------------------------------*
412100030911     c     Tab_EI        BEGSR
412200030911      *
412300030911     c                   clear                   dsEI
412400030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
412500030911     c                   movel     'EI'          TBLcod
412600030911     c                   movel(p)  DB0cei        TBLkey
412700030911     c     K03TAB        chain     TABEL
412800030911if  2c                   if        %found(TABEL00F)
412900030911     c                   movel     TBLuni        dsEI
413000030911e   2c                   endif
413100030911e   1c                   endif
413200030911     c                   eval      DB0dce =  §EIdes
413300030911      *
413400030911     c                   ENDSR
413500030515      *
413600030515      *---------------------------------------------------------------*
413700050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
413800030515      *---------------------------------------------------------------*
413900030515     c     Tab_1A        BEGSR
414000030515      *
414100030515     c                   clear                   ds1A
414200030515if  1c                   if        DB0cas <> *zeros
414300030515     c                   movel     '1A'          TBLcod
414400030515     c                   movel(p)  DB0tic        TBLkey
414500030515     c     K03TAB        chain     TABEL
414600030515if  2c                   if        %found(TABEL00F)
414700030515     c                   movel     TBLuni        ds1A
414800030515e   2c                   endif
414900030515e   1c                   endif
415000030515      *
415100030515      * Imposto i dati nella DS
415200030515     c                   eval      DB0dti = §1Ades
415300030515     c                   eval      DB0ucm = §1Aucm
415400030515     c                   eval      DB0ste = §1Aste
415500030515      *
415600030515     c                   ENDSR
415700030515      *
415800030515      *---------------------------------------------------------------*
415900050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
416000030515      *---------------------------------------------------------------*
416100030515     c     Tab_1P        BEGSR
416200030515      *
416300030515     c                   clear                   ds1P
416400030515     c                   movel     '1P'          TBLcod
416500130924     c***?già impostato:?movel(p)  DB0tc1/2      TBLkey
416600030515     c     K03TAB        chain     TABEL
416700030515if  1c                   if        %found(TABEL00F)
416800030515     c                   movel     TBLuni        ds1P
416900030515e   1c                   endif
417000030515      *
417100030515     c                   ENDSR
417200030515      *
417300030515      *---------------------------------------------------------------*
417400050217      *?Calcolo importi LDV                                          ?*
417500030515      *---------------------------------------------------------------*
417600061031     c     sr_DS_IMPO    BEGSR
417700030515      *
417800030515      * Vedo se esiste tassazione
417900030515      *  solo se tipo bolla contabilizzabile
418000061030if  1c                   IF        §TBfcb =  '1'
418100101213      *
418200061030     c                   eval      Wtrc   =  '1'
418300040607     c     KLDVtrc       chain     FIAR6000
418400101213      *
418500030515if  2c                   if        %found(FIAR601L)
418600030515     c                   movel     AR6div        DB0div
418700030515     c                   exsr      CtrDIV
418800030515      *
418900030515      * NON c'è 2ª bolla
419000101213if  3c                   if        DB0tb2 = *blanks
419100030515      *  Estremi fattura
419200030515     c                   z-add     AR6nft        DB0nft
419300030515     c                   z-add     AR6dft        DB0dft
419400130205     c                   z-add     AR6fiv        wB0fiv
419500101213      *  Se è assegnato => stampo tutta la tassaz.
419600101213if  4c                   if        %subst(DB0tb1:1:1) = 'A'
419700061030     c                   eval      Wtrc   =  '1'
419800030515     c                   exsr      RepTASSAZ
419900030515e   4c                   endif
420000030515e   3c                   endif
420100030515e   2c                   endif
420200030515      *
420300061030      * C'è 2ª bolla
420400101213if  2c                   IF        DB0tb2 <> *blanks
420500030515      *
420600061030     c                   eval      Wtrc   =  '2'
420700040607     c     KLDVtrc       chain     FIAR6000
420800030515      *
420900061030if  3c                   If        %found(FIAR601L)
421000030515     c                   movel     AR6div        DB0div
421100030515     c                   exsr      CtrDIV
421200030515     c                   movel     AR6cei        DB0cei
421300030515      *   Estremi fattura
421400030515     c                   z-add     AR6nft        DB0nft
421500030515     c                   z-add     AR6dft        DB0dft
421600130205     c                   z-add     AR6fiv        wB0fiv
421700030515if  4c                   if        AR6ksc <> *zeros
421800030515     c                   eval      DB0ccd =  AR6ksc
421900030515e   4c                   endif
422000061030     c                   eval      Wtrc   =  '2'
422100030515     c                   exsr      RepTASSAZ
422200061030e   3c                   EndIf
422300030515      *
422400030515e   2c                   ENDIF
422500030515      *
422600030515e   1c                   ENDIF
422700030515      *
422800030515      * Vedo se ha contrassegno
422900060505     c     KLDV          chain     FIAR9000
423000060505if  1c                   IF        %found(FIAR901L)
423100061030     c                   eval      DB0cas =  AR9cas
423200061030     c                   eval      DB0vca =  AR9vca
423300061030     c                   eval      DB0gca =  AR9gca
423400061030     c                   eval      DB0tic =  AR9tic
423500030515x   1c                   ELSE
423600030515     c                   clear                   DB0cas
423700030515     c                   clear                   DB0vca
423800030515     c                   clear                   DB0gca
423900030515     c                   clear                   DB0tic
424000030515     c                   clear                   DB0dti
424100030515     c                   clear                   DB0ucm
424200030515     c                   clear                   DB0ste
424300030515e   1c                   ENDIF
424400121018      *
424500131015      * Se P.A. con fattura immediata riporterò il Codice Fiscale
424600121018      *   (NON se destinatario estero)
424700121026if  1c                   if           ( %subst(DB0tb1:1:1) = 'A'
424800121018     c                             and (DB0nft  <> *zeros
424900121018     c                              or  DB0ift  <> *zeros)
425000121018     c                             and  DB0nzd  =  *blanks )
425100121018     c                   exsr      RepAR4_Q
425200121018     c                   movel     §TA4Qcfd      DB0cpi
425300121018e   1c                   endif
425400121018     c                   eval      wB0cfi = DB0cpi
425500061030      *
425600030515      * Vedo se si tratta di bolla figlia:
425700040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
425800070515if  1c                   if        $Mamma =  *on
425900030911     c                   exsr      BollaFiglia
426000030515e   1c                   endif
426100030515      *
426200030515     c                   ENDSR
426300030515      *
426400030515      *---------------------------------------------------------------*
426500050217      *?Decodifica LNA                                               ?*
426600030515      *---------------------------------------------------------------*
426700131203     c     sr_DecLNA     BEGSR
426800030515      *
426900030515if  1c                   if            DB0dla =  *blanks
427000030515     c                             and DB0ris <> 'M'
427100030515      *
427200040311     c                   eval      SAVlna = DB0lna
427300070312     c                   clear                   OG146arr
427400061016     c                   clear                   OG148arr
427500070312     c                   clear                   OG150arr
427600030515     c     DB0lna        chain     AZORG
427700030515      *
427800030515if  2c                   if        %found(AZORG01L)
427900030515     c                   eval      DB0fag = ORGfag
428000030515     c                   eval      DB0dla = ORGdes
428100030515     c                   eval      DB0inl = ORGind
428200030515     c                   eval      DB0cal = ORGcpf
428300030515     c                   eval      DB0lol = ORGloc
428400030515     c                   eval      DB0prl = ORGpro
428500030515     c                   eval      DB0tel = ORGtel
428600030515     c                   eval      DB0fax = ORGfax
428700070312     c                   movel     ORGde6        OG146arr
428800061016     c                   movel     ORGde8        OG148arr
428900070312     c                   movel     ORGdf0        OG150arr
429000030515e   2c                   endif
429100030515      *
429200030515e   1c                   endif
429300030515      *
429400030515     c                   ENDSR
429500030515      *
429600030515      *---------------------------------------------------------------*
429700050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
429800030515      *---------------------------------------------------------------*
429900030515     c     RepTASSAZ     BEGSR
430000030515      *
430100030515     c                   eval      DB0imv = AR6imv
430200030515     c                   eval      DB0cei = AR6cei
430300030515     c                   eval      DB0ift = AR6ift
430400101213      *
430500030911      * Esenzione IVA
430600030911if  1c                   if            DB0cei <> *blanks
430700030911     c                             and DB0dce =  *blanks
430800030911     c                   exsr      Tab_EI
430900030911e   1c                   endif
431000101213      *
431100101213      * Carico tabella VARIABILI ARRIVI
431200101213     c                   clear                   ds$2
431300101213     c                   eval      TBLcod = '$2'
431400101213     c                   eval      TBLkey = '1'
431500101213     c     K03TAB        chain     TABEL
431600101213if  1c                   if        %found(TABEL00F)
431700101213     c                   movel     TBLuni        ds$2
431800101213e   1c                   endif
431900030515      *
432000030515      * Controllo se ci sono varie esenti
432100030515      *
432200030515      * - varia 1
432300030515     c                   eval      Wsvx = AR6sv1
432400030515     c                   eval      Wvax = AR6va1
432500030515     c                   exsr      RepTASSAZ_b
432600030515      * - varia 2
432700030515     c                   eval      Wsvx = AR6sv2
432800030515     c                   eval      Wvax = AR6va2
432900030515     c                   exsr      RepTASSAZ_b
433000030515      * - varia 3
433100030515     c                   eval      Wsvx = AR6sv3
433200030515     c                   eval      Wvax = AR6va3
433300030515     c                   exsr      RepTASSAZ_b
433400030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
433500040607     c     KLDVtrc       setll     FIAR7000
433600040607     c     KLDVtrc       reade     FIAR7000
433700030515      *
433800030515do  2c                   Dow       Not %eof(FIAR701L)
433900030515      *
434000030515     c                   eval      Wsvx = AR7svn
434100030515     c                   eval      Wvax = AR7van
434200030515     c                   exsr      RepTASSAZ_b
434300030515      *
434400040607     c     KLDVtrc       reade     FIAR7000
434500030515e   2c                   Enddo
434600030515      *
434700030515     c                   ENDSR
434800030515      *
434900030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
435000050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
435100050217      *?- 2ª parte -                                                 ?*
435200030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
435300030515     c     RepTASSAZ_b   BEGSR
435400030515      *
435500030515sel 1c                   Select
435600030515      *
435700030515w   1c                   When      Wsvx       = *blanks
435800030515      *
435900030515w   1c                   When      Wsvx       = §$2iva
436000030515     c                   eval      DB0iva     = Wvax
436100030515      *
436200030515w   1c                   When      Wsvx       = §$2bol
436300030515     c                   add       Wvax          DB0ese
436400110117     c*//                z-add     Wvax          wBolloVirt
436500110117     c*//                eval      $BolloVirt = *on
436600030515      *
436700030515x   1c                   Other
436800101213      * Reperimento varie esenti IVA
436900101213     c                   movel     'CC'          TBLcod
437000101213     c                   movel(p)  Wsvx          TBLkey
437100101213     c     K03TAB        chain     TABEL
437200101213if  1c                   IF             %found(TABEL00F)
437300101213     c                             and  TBLflg = *blanks
437400101213     c                   movel     TBLuni        dsCC
437500101213if  2c                   if        §CCcei <> *blanks
437600101213     c                   add       Wvax          DB0ese
437700101213     c                   sub       Wvax          DB0imv
437800101213e   2c                   endif
437900101213e   1c                   ENDIF
438000030515      *
438100101213e   1c                   Endsl
438200030515      *
438300030515     c                   ENDSR
438400030515      *
438500030515      *---------------------------------------------------------------*
438600050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
438700030515      *---------------------------------------------------------------*
438800030515     c     TABxPAR       BEGSR
438900030515      *
439000100517     c                   movel(p)  WCdPar        w053a
439100030515     c                   movel     '7V'          TBLcod
439200030515     c                   movel(p)  KeyPAR        TBLkey
439300030515     c     K03TAB        chain     TABEL
439400030515      *
439500030515if  1c                   if        %found(TABEL00F)
439600030515     c                             and TBLflg = *blanks
439700100517     c                   eval      w053a = WCdPar + '-'
439800121108     c                                   + %trim(%subst(TBLuni:1:49))
439900030515x   1c                   else
440000030515     c                   clear                   TBLuni
440100030515e   1c                   endif
440200030515      *
440300030515     c                   ENDSR
440400030515      *
440500030515      *---------------------------------------------------------------*
440600050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
440700030515      *---------------------------------------------------------------*
440800030515     c     CtrCAs        BEGSR
440900030515      *
441000030515      * E' da convertire
441100030515if  1c                   if        DB0ucm = 'S'
441200030515     c                   eval      $CAeur = *on
441300030515e   1c                   endif
441400030515      *
441500030515      * Se in EURO è da convertire e da stampare in EURO
441600030515if  1c                   if        DB0vca = §GEDcn
441700030515     c                   eval      $CAeur = *on
441800030515     c                   endif
441900030515      *
442000030515      * Devo convertire il c/a da valuta a moneta di conto
442100030624if  1c                   if        DB0vca <> §GEDcn
442200030515     c                   clear                   YEURCODS
442300030515     c                   movel     DB0vca        YECdvi
442400030515     c                   z-add     DB0cas        YECimi
442500030515     c                   movel     §GEDcn        YECdvo
442600030515     c                   call      'YEURCO'
442700030515     c                   parm                    YEURCODS
442800030515if  2c                   if        YECesi = *blanks
442900030515     c                   z-add     YECimo        Wcas
443000030515e   2c                   endif
443100030515x   1c                   else
443200030515     c                   z-add     DB0cas        Wcas
443300030515e   1c                   endif
443400030515      *
443500030515     c                   ENDSR
443600030515      *
443700030515      *---------------------------------------------------------------*
443800050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
443900030515      *---------------------------------------------------------------*
444000030515     c     CtrDIV        BEGSR
444100030515      *
444200030515     c                   clear                   dsCV
444300030515      *
444400030515     c                   movel     'CV'          TBLcod
444500030515     c                   movel(p)  DB0div        TBLkey
444600030515     c     K03TAB        chain     TABEL
444700030515     c                   if        %found(TABEL00F)
444800030515     c                   movel     TBLuni        dsCV
444900030515     c                   movel     §CVfdc        DB0fdc
445000030515     c                   endif
445100030515      *
445200030515     c                   ENDSR
445300030515      *
445400030515      *---------------------------------------------------------------*
445500050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
445600030515      *---------------------------------------------------------------*
445700030911     c     BollaFiglia   BEGSR
445800030515      *
445900090619     c                   if        not %open(FNLBL01L)
446000101213     c                   open(e)   FNLBL01L
446100101213      /free
446200101213           if  %error;
446300101213             exsr  sr_ErrOpenFile;
446400101213           endif;
446500101213      /end-free
446600090619     c                   endif
446700090619      *
446800040607     c     KLDV          chain     FNLBL000
446900030515if  1c                   if        %found(FNLBL01L)
447000030515      *
447100030515     c     KLBL          chain     FNARB000
447200030515if  2c                   if        %found(FNARB01L)
447300030515      *
447400031015     c                   eval      wB0nrs = ARBnrs
447500030515     c                   eval      DB0fns = ARBfns
447600030515     c                   eval      DB0fls = ARBfls
447700030515     c                   eval      DB0ncd = ARBncd
447800030515     c                   eval      DB0nca = ARBnca
447900050114     c**!!!              eval      DB0xco = ARBxco
448000030515     c                   eval      DB0znc = ARBznc
448100030515      * Aggancio AR9
448200060505     c     KLBL          chain     FIAR9000
448300060505if  3c                   IF        %found(FIAR901L)
448400030515     c                   eval      DB0cas = AR9cas
448500030515     c                   eval      DB0vca = AR9vca
448600030515     c                   eval      DB0gca = AR9gca
448700030515     c                   eval      DB0tic = AR9tic
448800030515x   3c                   ELSE
448900030515     c                   clear                   DB0cas
449000030515     c                   clear                   DB0vca
449100030515     c                   clear                   DB0gca
449200030515     c                   clear                   DB0tic
449300030515     c                   clear                   DB0dti
449400030515     c                   clear                   DB0ucm
449500030515     c                   clear                   DB0ste
449600030515e   3c                   ENDIF
449700030515      *
449800030515e   2c                   endif
449900030515      *
450000030515e   1c                   endif
450100030515      *
450200030515     c                   ENDSR
450300030903      *
450400030515      *---------------------------------------------------------------*
450500050217      *?DEFINIZIONE KEY-LIST                                         ?*
450600030515      *---------------------------------------------------------------*
450700030515     c     DefKLIST      BEGSR
450800030515      *
450900090619      * - FNARB01L / FNART01L /
451000030515      *   FNLBL01L /
451100060505      *   FIAR501L/31C / FIAR901L
451200040607     c     KLDV          klist
451300030515     c                   kfld                    DB0aas
451400030515     c                   kfld                    DB0lnp
451500030515     c                   kfld                    DB0nrs
451600030515     c                   kfld                    DB0nsp
451700060505      * - FIAR401L / FIAR601L / FIAR701L
451800060505     c     KLDVtrc       klist
451900060505     c                   kfld                    DB0aas
452000060505     c                   kfld                    DB0lnp
452100060505     c                   kfld                    DB0nrs
452200060505     c                   kfld                    DB0nsp
452300060505     c                   kfld                    Wtrc
452400040607      * - FIAR501L/31C
452500030717     c     K05AR501      klist
452600030717     c                   kfld                    DB0aas
452700030717     c                   kfld                    DB0lnp
452800030717     c                   kfld                    DB0nrs
452900030717     c                   kfld                    DB0nsp
453000030717     c                   kfld                    AR5trd
453100090619      * - FNARB01L / FIAR901L di bolla MAMMA
453200070514     c     KLBL          klist
453300070514     c                   kfld                    LBLaap
453400070514     c                   kfld                    LBLlpp
453500070514     c                   kfld                    LBLnrp
453600070514     c                   kfld                    LBLnsp
453700081030      * - FNART01L di bolla MAMMA
453800081030     c     KLBLnsc       Klist
453900081030     c                   kfld                    LBLaap
454000081030     c                   kfld                    LBLlpp
454100081030     c                   kfld                    LBLnrp
454200081030     c                   kfld                    LBLnsp
454300081030     c                   Kfld                    Wnsc
454400081030     c     K05ARS01A     klist
454500081030     c                   kfld                    ARTfls
454600081030     c                   kfld                    ARTlna
454700081030     c                   kfld                    ARTnrs
454800081030     c                   kfld                    ARTnsc
454900081030     c                   kfld                    ARStrc
455000081030      * - FNART01L
455100081030     c     KLDVnsc       Klist
455200081030     c                   Kfld                    DB0aas
455300081030     c                   Kfld                    DB0lnp
455400081030     c                   Kfld                    DB0nrs
455500081030     c                   Kfld                    DB0nsp
455600081030     c                   Kfld                    Wnsc
455700100517      * - FNORM01L
455800100517     c     K04ORM01      klist
455900100517     c                   kfld                    ORMpoe
456000100517     c                   kfld                    ORMnsr
456100100517     c                   kfld                    ORMnor
456200100517     c                   kfld                    ORMnrv
456300030515      * - TABEL00F
456400030515     c     K03TAB        klist
456500040223     c                   kfld                    TBLkut
456600030515     c                   kfld                    TBLcod
456700030515     c                   kfld                    TBLkey
456800030515      *
456900030515     c                   ENDSR
457000080430
457100080430       //--------------------------------------------------------------
457200140303       //?Operazioni finali                                            ?
457300080430       //--------------------------------------------------------------
457400080430     c     sr_End        BEGSR
457500080430      *
457600080430     c                   clear                   TIBS02ds
457700080430     c                   eval      T02tla = 'C'
457800080430     c                   call      'TIBS02R'
457900080430     c                   parm                    KPJBA
458000080430     c                   parm                    TIBS02ds
458100080430     c                   reset                   FNLV80ds
458200080430     c                   call      'FNLV80R'
458300080430     c                   parm                    KPJBA
458400080430     c                   parm                    FNLV80ds
458500080430      /free
458600101222         if  §VPOrcm <= wDate and
458700101222             DB0tsp   = 'E'   and
458800101222            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
458900101222                                   DB0lnp <> 006   and
459000101222                                   DB0lnp <> 063));
459100101222           reset  TNSD99ds;
459200101222           tnsd99r ( TNSD99ds );
459300110117         endif;
459400080724
459500101222         // -?Chiusura del work-file?- - - - - - - - - - - - - - - - -*
459600090210
459700110202         if  %open(FIILV01L);
459800110202           close  FIILV01L;
459900090210         endif;
460000080430
460100101222         // -?Restituzione parametri?- - - - - - - - - - - - - - - - -*
460200080508
460300110221         if wCod_Log  <> '01';
460400130206           RootFolder = §ILVimgSca;
460500110221         endif;
460600110221
460700080520         if Risultato <> 'OK';
460800080718
460900080731           clear FileName;
461000080731           clear TemplateName;
461100080508
461200130213           for wx = 1 to c_Row;
461300130206             %occur(LdV) = wx;
461400130206             LdvPrg      = wx;
461500130206           endfor;
461600080508
461700080508         endif;
461800080430
461900131203
462000080430           exec sql   set result sets
462100080430                      for return to client
462200081103                      array :LdV   for :w_row rows;
462300080430
462400080430           select;
462500080430             when sqlCode = 100;
462600080430               RtnStatus  = sqlCode;
462700130924               //RtnOpCode  = 'SQLCODE   ';
462800130924               //RtnEsito   = -1;
462900080430             when sqlCode < *zero;
463000080430               RtnStatus  = sqlCode;
463100130924               //RtnOpCode  = 'SQLCODE   ';
463200130924               //RtnEsito   = -1;
463300080430           endsl;
463400080430
463500080430
463600080430         return;
463700080430
463800080430      /end-free
463900080430     c                   ENDSR
464000080430
464100080430      *---------------------------------------------------------------*
464200130227
464300140303       //--------------------------------------------------------------
464400140303       //?Spool di stampa (per e-mail).                                ?
464500140303       //--------------------------------------------------------------
464600130227
464700130227     oPrtEMAIL  e            EmlTxt            2
464800130227     o                       SDSpgm
464900130227     o                                        +   1 '-'
465000130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
465100130227     o                                              ABORATA 100 VOLTE'
465200130227     o          e            EmlTxt      0
465300130227     o                       SDSpgm
465400130227     o                                        +   1 '-'
465500130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
465600130227     o                                              ABORATA 100 VOLTE'
465700130227     o          e            EmlTxt      2
465800130227     o                                              'Spedizione     '
465900130227     o                                        +   3 'Distinta  '
466000130227     o          e            EmlTxt      1  1
466100130227     o                                              '------------------
466200130227     o                                              --'
466300130227     o                                        +   3 '----------'
466400130227     o          e            EmlDet      1
466500130227     o                       DB0aas        3
466600130227     o                       DB0lnp        3  +   1
466700130227     o                       DB0nrs        3  +   1
466800130227     o                       DB0nsp        3  +   1
466900130227     o                       P_Fgs2        3  +   3
467000130227     o                       P_Ndc2        3  +   1
467100130227     o          e            EmlEnd      2
467200130227     o                                              '***  Fine Lista  ***'
467300130227     o          e            EmlEnd      0
467400130227     o                                              '***  Fine Lista  ***'
467500080724
467600140303       //--------------------------------------------------------------
467700140303       //?Spool per immagine della LdV.                                ?
467800140303       //--------------------------------------------------------------
467900080430     oQSYSPRT   e            PRTtxt            3
468000080430     o                                              '   '
468100080430     o                       c_Char           +   0
468200080430     o          e            PRTrow      1
468300080430     o                       LDV              +   0
468400110201     o          e            PRTend      3
468500110201     o                       LastRow          +   0
468600080430
468700080718       //--------------------------------------------------------------
468800140303       //?Procedure name: getCurrentSystemName.                        ?
468900140303       //?Purpose:        Reperisce l'attributo di rete current System Name?
469000140303       //?Returns:        Current System Name                          ?
469100080718       //--------------------------------------------------------------
469200080718     p getCurrentSystemName...
469300080718     p                 b
469400080718
469500080718     d getCurrentSystemName...
469600080718     d                 pi             8a
469700080718
469800080718      /free
469900080718
470000080718       clear qUSEC;
470100080718       qUSBPRV = %size(qUSEC);
470200080718
470300080718       RetrieveNetworkAttributes( QWCRNETA_dataReturned
470400080718                                : %size(qWCRNETA_dataReturned)
470500080718                                : 1
470600080718                                : 'SYSNAME   '
470700080718                                : qUSEC
470800080718                                );
470900080718
471000080718       if  qUSEI <> *blank   or
471100080718           qWCRNETA_dataReturned.qWCRDR.qWCNBRAR = *zero;
471200080718         dump(A);
471300080718         clear qWCRNETA_networkAttributeInformationTable;
471400080718       else;
471500080718         qWCRNETA_networkAttributeInformationTable =
471600080718                                       qWCRNETA_dataReturned.qWCARRAY;
471700080718       endif;
471800080718
471900080718       return  qWCRNETA_networkAttributeInformationTable.qWCDATA;
472000080718
472100080718      /end-free
472200080718
472300080718     p getCurrentSystemName...
472400080718     p                 e
472500080718
472600110202       //--------------------------------------------------------------
472700110202       //?Schiere a tempo di compilazione.                             ?
472800110202       //--------------------------------------------------------------
472900030515
473000030515** > $GgS/$Gio * --------------------------------------------------------------*
4731000306241LUNEDI
4732000306242MARTEDI
4733000410263MERCOLEDI
4734000306244GIOVEDI
4735000306245VENERDI
4736000305156SABATO
4737000305157DOMENICA
473800030515M          MATT.
473900030515P          POM.
474000110202** -?$CoL?(Codice LOG)?/?$DeL?(Decodifica codice LOG)?
47410011020200Spedizione in distinta chiusa - da elaborare
47420011020201Spedizione elaborata
47430011032808Sped. non consegnata in dist. - immagine a SCARTO
47440011032509LdV da scannerizzare - immagine a SCARTO
47450011020321Parametri di input errati - immagine NON creata
47460011020822Distinta non trovata/annullata - immagine a SCARTO
47470011020823Sped./Dist. non trovata in FIILV - immag. a SCARTO
47480011020325Sped. non trovata in FNARB - immagine NON creata
47490013031351BOLLA RECUPERO - Immagine già generata
47500013031352BOLLA RECUPERO - Spedizione non trovata in FNARB
