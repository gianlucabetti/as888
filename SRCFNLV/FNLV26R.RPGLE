000100920914     H DECEDIT('0,') DATEDIT(*DMY.)
000200950321?     *--------------------------------------------------------------*
000300071113?     *     RICHIESTA AGGIORNAMENTO FOR.FATTURAZ.                    *
000400950321?     *--------------------------------------------------------------*
000900071113     FFNLv26D   CF   E             WORKSTN
000901071113     Fansog01l  IF   E           K DISK
000902071113     Fanfrn01l  IF   E           K DISK
000906071113     d trulcffds     e ds
000907071113     d fnlv26ds      e ds
000908071113     d kpjba         e ds
000909071113     d savkscfat       s                   like(frnforfatt)
024100061012     c*---------------------------------------------------------------*
024200061012     c* FLUSSO PRINCIPALE                                             *
024300061012     c*---------------------------------------------------------------*
024301071113     c     krco          chain     anfrn01l
024302071113     c                   if        not %found(anfrn01l)
024303071113     c                   eval      d26err = '1'
024304071113     c                   else
024305071113     c                   eval      savkscfat = frnforfatt
024306071113     c     frnsogg       chain     ansog01l
024307071113     c                   if        not %found(ansog01l)
024308071113     c                   eval      d26err = '1'
024309071113     c                   else
024310071113     c                   eval      wdesksc = sogdes
024311071113     c                   eval      wsoc = d26soc
024312071113     c                   eval      wksc = d26ksc
024313071113     c* reperisco il fornitore di fatturazione
024314071113     c                   clear                   trulcffds
024315071113     c                   eval      icfftpa = 'C'
024316071113     c                   exsr      srcff
024317071113     c* se il for. fattuz. è uguale a quello trovato esco senza emettere
024318071113     c* il video
024319071114     c                   if        *in99 or (ocffkscfat <> savkscfat
024320071114     c                             and ocffkscfat <> d26ksc)
024321071114     c* decodifico il fornitore di fatturazione se diverso dal codice for.
024322071114     c                   eval      wfat = ocffkscfat
024323071114     c                   if        ocffkscfat <> d26ksc
024324071114     c     krcofat       chain     anfrn01l
024325071114     c                   if        %found(anfrn01l)
024326071114     c                   eval      wdesfat = sogdes
024327071114     c                   else
024328071114     c                   eval      wdesfat = wdesksc
024329071114     c                   end
024330071114     c                   end
024331071114     c*
024332071114     c                   eval      waggio = 'SI'
024333071114     c                   if        *in99
024334071114     c                   eval      waggio ='NO'
024335071114     c                   end
024336071113     c*
024337071113     c                   do        *hival
024338061012     C                   EXFMT     LRZ1W01
024339071113     C*
024340071113     c                   if        *inkl
024341071113     c                   eval      d26err = '1'
024342071113     c                   leave
024343071113     c                   end
024353071113     c*
024354071113     c                   if        waggio = 'SI' and not *in99
024355071114     c                             and *inkf
024357071113     c                   clear                   trulcffds
024358071113     c                   eval      icfftpa = 'A'
024359071113     c                   exsr      srcff
024362071113     c                   leave
024363071113     c                   end
024364061012     c                   enddo
024365071113     c                   end
024366071113     c                   end
024367071113     c                   end
024368061012     c*
024369071113     C                   MOVEL     FNLV26ds      KPJBU
024370061012     C                   SETON                                        LR
175200061012     c*---------------------------------------------------------------*
175300071113     c     srcff         begsr
175400061012     c*---------------------------------------------------------------*
175401071113     c                   setoff                                       99
175402071113     c                   eval      icffsoc = d26soc
175403071113     c                   eval      icffksc = d26ksc
175404071113     c                   call      'TRULCFFR'
175405071113     c                   parm                    trulcffds
175406071113     c                   if        ocfferr <> ' '
175407071114     c                   eval      wmsg = 'Err:for.fatturaz.-
175408071114     c                              avvisare uff. IVA'
175409071113     c                   seton                                        99
175410071113     c                   end
175411071113     c                   endsr
175412071113     c*---------------------------------------------------------------*
175413071113     c     *inzsr        begsr
175414071113     c*---------------------------------------------------------------*
175500061012     c*  Operazioni iniziali
175600061012     C     *ENTRY        PLIST
175700061012     C                   PARM                    KPJBA
175900071113     C                   MOVEL     KPJBU         FNLv26ds
175901071113     c                   clear                   d26err
176840071113     C     Krco          KLIST
176841071113     C                   KFLD                    d26soc
176842071113     C                   KFLD                    d26ksc
176843071113     C     Krcofat       KLIST
176844071113     C                   KFLD                    d26soc
176845071113     C                   KFLD                    ocffkscfat
176900061012     c                   endsr
