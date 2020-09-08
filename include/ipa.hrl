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
%
% You should have received a copy of the GNU General Public License along
% with this program; if not, write to the Free Software Foundation, Inc.,
% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

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
