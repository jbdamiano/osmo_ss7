% (C) 2020 by Matt Johnson <matt9j@cs.washington.edu>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

-ifndef(IPA).
-define(IPA, true).

-record(ipa_ccm_options, {serial_number,
			  unit_id,
			  mac_address,
			  location,
			  unit_type,
			  equipment_version,
			  sw_version,
			  unit_name,
			  initiate_ack=false}).

-endif.
