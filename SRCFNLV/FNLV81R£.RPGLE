000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200110523     H* FNLV79R *----------------------------------------------------*
000300110523     H* - Interrogazione Email Destinatario
000301110610     h*
000302110610     h*   Prevede l'interrogazione sia da sede che da filiale ma al momento
000303110610     h*   quella da sede non serve in quanto in fase di ricezione il record
000304110610     h*   "EMD" non viene tenuto ma serve solo per aggiornare flag su titas
000400000000     H*--------------------------------------------------------------*
000500110603     FFiar501L  IF   E           K DISK    usropn
000501110603     f                                     rename(fiar5000:fiar5fil)
000502110603     FFiar531c  IF   E           K DISK    usropn
001000110524     FFNLv79D   CF   E             WORKSTN
001200030722     F**
001300070313     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
001600070313     d
001700051123      * ds di passaggio dati
001800110523     D fnlv79ds      E DS
002000070313     d
002100110524     D Dar5emd       E DS
002400070313     d
003400110524     d Ktrd            s                   like(ar5trd) inz('EMD')
003401110524     d Dataeur         s               d   datfmt(*eur)
003800900518     C***************************************************************
003900000000     C     *ENTRY        PLIST
004000110523     C                   PARM                    fnlv79ds
004300940223     C*---------------------------------------------------------------*
004400070326     C* TIPO LANCIO, ilrG6TLA "C" -           CHIUSO CON LR
004500070326     C* TIPO LANCIO, ilrG6TLA "L" - ELABORO E CHIUDO CON LR
004600070326     C* TIPO LANCIO, ilrG6TLA " " - ELABORO E CHIUDO IN RETRN
004700941216     C*
004800110524    1C     ilv79TLA      IFNE      'C'
004900110524     c                   clear                   olv79err
005000110524     c                   clear                   olv79msg
005400070412     c
005500051123     c
005900070313     c
005901110603     c                   if        %open(fiar501l)
006000110524     c     kar5          chain     fiar501l
006001110603     c                   else
006002110603     c     kar5          chain     fiar531c
006003110603     c                   endif
006100110603     c                   if        not %found
006200070313     c* restituisco errore al chiamante ed esco
006300110524     c                   eval      olv79msg=msg(1)
006400110524     c                   eval      olv79err='E'
006500070313     c                   goto      fine
006600070313     c                   endif
007000070326     c
008700110524     c*
008800110524     c                   movel     ar5uni        dar5emd
008801130927     c                   setoff                                       414243
008802130927     c* Indirizzo Email se presente
008803130927    1c                   if        §ar5eml<>*blanks
008804130930     c                   eval      w1cdemd='MAIL:'
008805110524     c                   eval      w1cemd=§ar5eml
008806130927    2c                   if        §ar5mmp='S'
008807130927     c                   seton                                        41
008808130930     c                   eval      w1cddto='Data invio'
008809130927    3c                   if        §ar5dto>*zeros
008810130927     c
008811110614     c                   movel     §ar5dto       w0080             8 0
008812110614     c                   eval      dataeur=%date(w0080:*iso)
008813110614     c                   eval      v1cdat= %dec(dataeur:*eur)
008814110614     c                   move      §ar5dto       w1cora
008815130927    3c                   endif
008816130927    2c                   endif
008817130927    1c                   endif
008818130927     c* Cellulare se presente. Se non c'è indirizzo email lo metto nei campi
008819130927     c* mail altrimenti riempo campi cellulare
008820130927    1c                   if        §ar5tel<>*blanks
008821130927    2c***                if        §ar5eml=*blanks
008822130927     c***                eval      w1cdemd='Cellulare:'
008823130927     c***                eval      w1cemd=§ar5tel
008824130927    3c***                if        §ar5smp='S'
008825130927     c***                seton                                        41
008826130930     c***                eval      w1cddto='Data invio'
008827130927    4c***                if        §ar5dos>*zeros
008828130927     c***                movel     §ar5dos       w0080             8 0
008829130927     c***                eval      dataeur=%date(w0080:*iso)
008830130927     c***                eval      v1cdat= %dec(dataeur:*eur)
008831130927     c***                move      §ar5dos       w1cora
008832130927    4c***                endif
008833130927    3c**                 endif
008834130927   x2c***                else
008835130927     c                   seton                                        42
008836130927     c                   eval      w1ctel=§ar5tel
008837130927    3c                   if        §ar5smp='S'
008838130927     c                   seton                                        43
008839130927    4c                   if        §ar5dos>*zeros
008840130927     c                   movel     §ar5dos       w0080             8 0
008841130927     c                   eval      dataeur=%date(w0080:*iso)
008842130927     c                   eval      W1cdas= %dec(dataeur:*eur)
008843130927     c                   move      §ar5dos       w1cors
008844130927    4c                   endif
008845130927    3c                   endif
008846130927    2c****               endif
008847130927    1c                   endif
008848110524     c                   exfmt     lv79w01
008900070326     c
011700070314     c
011800070314     c     fine          TAG
011900070314     c
012000070314     c                   endif
012100051123     c
012101131120     c                   writE     LV79ASS
012200110524     C     Ilv79tla      IFNE      ' '
012400051123     C                   SETON                                        LR
012500051123     C                   ELSE
012600051123     C                   SETON                                        RT
012700051123     C                   ENDIF
012800991206     C**************************************************************************
012900051123     C     *INZSR        BEGSR
013000051123     c
013100110524     c     kAR5          Klist
013200110524     c                   Kfld                    ilv79Aas
013300110524     c                   Kfld                    ilv79Lnp
013400110524     c                   Kfld                    ilv79nrs
013500110524     c                   Kfld                    ilv79nsp
013501110524     c                   Kfld                    ktrd
013502110603     c
013503110603     c* Apro file di filiale e se non c'e' quello di sede
013505110603     c                   open(e)   fiar501l
013506110603     c                   if        %error
013507110603     c                   open      fiar531c
013508110603     c                   endif
015300070313     c                   endsr
052200070313**
052300110524Indirizzo Email destinatario inesistente
