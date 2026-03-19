/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */
Module['cast'] = function(name, message) {
    ccall("cast", 'void', ['string', 'string'], [name, message]);
};
Module['call'] = async function(name, message) {
    const promiseId = ccall("call", 'integer', ['string', 'string'], [name, message]);
    return promiseMap.get(promiseId).promise;
};
Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function() {
    if (typeof ENVIRONMENT_IS_WEB === 'undefined' || !ENVIRONMENT_IS_WEB) {
        return;
    }

    const moduleArgs = (Module['arguments'] || []).slice();
    if (!moduleArgs.length) {
        return;
    }

    addRunDependency('atomvm-nosmp-preload');

    const preloadPath = async function(path) {
        const response = await fetch(path);
        if (!response.ok) {
            throw new Error('Failed to preload ' + path + ': HTTP ' + response.status);
        }

        const slashIndex = path.lastIndexOf('/');
        if (slashIndex > 0) {
            FS.mkdirTree(path.substring(0, slashIndex));
        }

        const data = new Uint8Array(await response.arrayBuffer());
        FS.writeFile(path, data, { canOwn: true });
    };

    Promise.all(moduleArgs.map(preloadPath)).then(function() {
        removeRunDependency('atomvm-nosmp-preload');
    }).catch(function(err) {
        console.error(err);
        abort(err && err.message ? err.message : String(err));
    });
});
