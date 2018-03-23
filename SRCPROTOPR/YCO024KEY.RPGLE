000100070703     D FORMATOKEY_ELENCO_FATTURE...
000200070703     D                 C                   'ELENCOFATT'
000300070703     D FORMATOKEY_NUOVA_FATTURA...
000400070703     D                 C                   'NUOVAFATT'
000500070703     D FORMATOKEY_DETTAGLIO_FATTURA...
000600070703     D                 C                   'DETTAGLIOF'
000700070703     D FORMATOKEY_TESTATA_FATTURA...
000800070703     D                 C                   'TESTATAFAT'
000900070703     D FORMATOKEY_DESCRIZIONI_LIBERE...
001000070703     D                 C                   'DESLIBERE'
001100070529
001200070529     D*--------------------------------------------------
001300070703     D* Procedure name: FatturaManuale_key_newTasti
001400070529     D* Purpose:        Imposta i tasti usabili nell'elenco delle fatture.
001500070529     D* Returns:        Esito.
001600070703     D* Parameter:      piFormato =>
001700070703     D* Parameter:      piOperazioneDisabilitata => Azioni da disabilitare.
001800070620     D* Parameter:      piOperazioneReverseImage => Azioni da mettere in reverse image.
001900070529     D*--------------------------------------------------
002000070703     D FatturaManuale_key_newTasti...
002100070529     D                 PR            10I 0
002200070703     D  piFormato...
002300070703     D                               10A   CONST
002400070620     D  piOperazioneDisabilitata...
002500070530     D                               15A   DIM(32)
002600070529     D                                     OPTIONS(*NOPASS:*OMIT)
002700070620     D  piOperazioneReverseImage...
002800070530     D                               15A   DIM(32)
002900070529     D                                     OPTIONS(*NOPASS:*OMIT)
003000070530
003100070530     D*--------------------------------------------------
003200070703     D* Procedure name: FatturaManuale_key_getTasti
003300070530     D* Purpose:        Restituisce 2 variabili contenenti la descrizione
003400070530     D*                          dei tasti.
003500070530     D* Returns:        Esito.
003600070530     D* Parameter:      key1 => Variabile contenente la prima riga tasti funzionali.
003700070530     D* Parameter:      key2 => Variabile contenente la seconda riga tasti funzionali.
003800070530     D*--------------------------------------------------
003900070703     D FatturaManuale_key_getTasti...
004000070530     D                 PR            10I 0
004100070703     D  piFormato...
004200070703     D                               10A   CONST
004300070703     D  key1                         79A   OPTIONS(*VARSIZE)
004400070703     D  key2                         79A   OPTIONS(*VARSIZE)
004500070703     D  pil01                         3P 0 VALUE OPTIONS(*NOPASS)
004501070703     D  pil02                         3P 0 VALUE OPTIONS(*NOPASS)
004502070703
004600070529     D*--------------------------------------------------
004700070703     D* Procedure name: FatturaManuale_key_isTastoAttivo
004800070529     D* Purpose:        Restituisce *ON se il tasto premuto è attivo.
004900070529     D* Returns:        *ON = tasto attivo.
005000070529     D* Parameter:      piDspAid => AID byte.
005100070529     D*--------------------------------------------------
005200070703     D FatturaManuale_key_isTastoAttivo...
005300070529     D                 PR              N
005400070703     D  piFormato...
005500070703     D                               10A   CONST
005600070529     D  piDspAid                      1A
005700070620     D  piOperazione                 15A
005800070620
